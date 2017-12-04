import "./main.css"
import logoPath from "./logo.svg"
const { App } = require("./App.elm")
const { Welcome } = require("./Welcome.elm")
import LZString from "lz-string"
import firebase from "firebase"

const getParameterByName = (name_, url) => {
    if (!url) url = window.location.href
    let name = name_.replace(/[\[\]]/g, "\\$&")
    let regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
        results = regex.exec(url)
    if (!results) return null
    if (!results[2]) return ""
    return decodeURIComponent(results[2].replace(/\+/g, " "))
}

const firebase_config = require("../secrets/firebase-app-config.json")
firebase.initializeApp(firebase_config)
const gamesRootRef = firebase.database().ref("games/")

const gameId = getParameterByName("game_id")
console.log("gameId: ", gameId)

if (gameId === null || gameId.trim() === "") {
    const createNewGame = welcomeApp => {
        gamesRootRef.push({ other_player: "waiting" }).then(data => {
            console.log("  >> data: ", data.key)
            welcomeApp.ports.newGameCreated.send(data.key)

            data.on("value", state => {
                console.log("  >> state: ", state.val())
                if (state.val().other_player === "joined") {
                    window.location.href = `/?game_id=${data.key}`
                }
            })
        })
    }

    let welcomeApp = Welcome.embed(document.getElementById("root"), logoPath)
    welcomeApp.ports.createNewGame.subscribe(() => createNewGame(welcomeApp))
} else {
    let app = App.embed(document.getElementById("root"), logoPath)
    gamesRootRef.child(gameId).set({ other_player: "joined" })
    app.ports.alert.subscribe(str => window.alert(str))
}

// const base64ToArrayBuffer = (base64) => {
//     var binary_string =  window.atob(base64);
//     var len = binary_string.length;
//     var bytes = new Uint8Array( len );
//     for (var i = 0; i < len; i++)        {
//         bytes[i] = binary_string.charCodeAt(i);
//     }
//     return bytes;
// }
//
// const arrayBufferToBase64 = ( buffer ) => {
//     var binary = '';
//     var bytes = new Uint8Array( buffer );
//     var len = bytes.byteLength;
//     for (var i = 0; i < len; i++) {
//         binary += String.fromCharCode( bytes[ i ] );
//     }
//     return window.btoa( binary );
// }

// const str = '{"h": [Helllo world worlds dfvfdfb gf bgf g g g gf gf g g gh gh hg h h gh]}'
// const b64 = window.btoa(str)
// console.log(str.length, b64.length)
// console.log(window.atob(LZString.decompress(LZString.compress(b64))))
