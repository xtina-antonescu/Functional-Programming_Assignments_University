import express from "express";
import cors from "cors";
import fs from "fs";

function forecast(req: express.Request, res: express.Response) {
  const resp = JSON.parse(fs.readFileSync("./scripts/data/response.json", { encoding: "utf-8" }));
  res.status(200).json(resp);
}

interface Args {
  slow: boolean;
}

function main(args: Args) {
  const app = express();
  app.use(cors());
  if (args.slow) {
    app.use((req, res, next) => {
      setTimeout(() => {
        next();
      }, 1000);
    });
  }

  const port = 3000;

  app.get("/v1/forecast", forecast);

  const server = app.listen(port, () => {
    console.log(`Starting server ${port}`);
    if (args.slow) {
      console.log(`Slow mode ON. Delay: 1 second.`);
    }
  });
}

function parseArgs() {
  const args: Args = {
    slow: ["--slow"].includes(process.argv[2]),
  };
  main(args);
}

if (typeof require !== "undefined" && require.main === module) {
  parseArgs();
}
