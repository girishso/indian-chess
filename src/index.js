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
    return decodeURIComponent(results[2].replace(/\+/g, " ").trim())
}

const compress = str => LZString.compressToUTF16(str)
const decompress = str => LZString.decompressFromUTF16(str)

const firebase_config = require("../secrets/firebase-app-config.json")
firebase.initializeApp(firebase_config)
const gamesRootRef = firebase.database().ref("games/")

const gameId = getParameterByName("game_id")
console.log("gameId: ", gameId)

if (gameId === null) {
    const createNewGame = app => {
        gamesRootRef.push({ other_player: "waiting" }).then(data => {
            console.log("  >> data: ", data.key)
            window.location.href = `/?game_id=${data.key}`
        })
    }

    let welcomeApp = Welcome.embed(document.getElementById("root"), logoPath)
    welcomeApp.ports.createNewGame.subscribe(() => createNewGame(welcomeApp))
} else {
    let app = App.embed(document.getElementById("root"),
      [`${window.location.origin}/?game_id=${gameId}`, "WhitePlayer"])

    gamesRootRef.child(`${gameId}/nPlayers`).transaction(nPlayers => {
      console.log("  >>> nPlayers: ", nPlayers)

      let newNPlayers = (nPlayers || 0)
      if(newNPlayers < 2)
        return newNPlayers += 1

      return
    }, (e, commited, snapshot) => {
      console.log(commited, snapshot.val())
      if(commited && snapshot.val() == 2) app.ports.setThisPlayer.send("BlackPlayer")
    }, false)



    gamesRootRef.child(gameId).on("value", state => {
        const json = state.val()
        console.log("  >> joined state: ", json)
        if(json.nPlayers >= 2) {
            app.ports.newSharedGameCreated.send(`${window.location.origin}/?game_id=${gameId}`)
        }
        if (typeof json.game_state !== "undefined" && json.game_state !== null) {
            let uncmpd = decompress(json.game_state)
            // console.log("  >> uncmpd: ", uncmpd)
            app.ports.gameStateChanged.send(JSON.parse(uncmpd))
        }
    })
    app.ports.sendGameState.subscribe(str => {
        let cmpd = compress(str)
        gamesRootRef.child(gameId).update({ game_state: cmpd })
    })
    app.ports.alert.subscribe(str => window.alert(str))
}
