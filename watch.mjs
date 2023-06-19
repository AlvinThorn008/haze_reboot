import fs from "fs/promises";
import { spawn, execFile } from "child_process";
import { promisify } from "util";

function sleep(ms) {
    return new Promise((resolve) => {
        setTimeout(resolve, ms);
    });
}

const controller = new AbortController();
const { signal } = controller;

// exec("fx", [],  { signal });

let fx = spawn("fx", ["ast.json"], {
    stdio: 'inherit'
});

const watcher = fs.watch("./prog_file.txt", { signal });

try {
    for await (const event of watcher) {
        if (event.filename && event.eventType == "change") {
            await sleep(100);
            console.log("prog_file.txt changed");
            await execFile("./target/debug/haze_reboot.exe");
    
            fx.kill();
            fx = spawn("fx", ["ast.json"], {
                stdio: 'inherit'
            });
        }
    }
} catch (err) {
    console.log(err);
}


// fs.watch("./prog_file.txt", (event, filename) => {
//     if (filename && event == "change") {
//         if (fsWait) return;
//         fsWait = setTimeout(() => {
//             fsWait = false;
//         }, 100);

//         console.log("prog_file.txt changed");
//         execFile("./target/debug/haze_reboot.exe", );
//         spawn("fs", ["ast.json"], {
//             stdio: "inherit"
//         })
//         // controller.abort();
//         // queueMicrotask(() => {
//         //     exec("fx ast.json", { signal });
//         // })
//     }
// })