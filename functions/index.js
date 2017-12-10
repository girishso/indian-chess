const functions = require("firebase-functions")
const admin = require("firebase-admin")
const secureCompare = require("secure-compare")

admin.initializeApp(functions.config().firebase)

// // Create and Deploy Your First Cloud Functions
// // https://firebase.google.com/docs/functions/write-firebase-functions
//
exports.helloWorld = functions.https.onRequest((request, response) => {
    response.send("Hello from Firebase!")
})

exports.deleteOldGames = functions.https.onRequest((req, res) => {
    const key = req.query.key

    // Exit if the keys don't match
    if (!secureCompare(key, functions.config().cron.key)) {
        console.log(
            "The key provided in the request does not match the key set in the environment. Check that",
            key,
            "matches the cron.key attribute in `firebase env:get`"
        )
        res
            .status(403)
            .send(
                'Security key does not match. Make sure your "key" URL query parameter matches the ' +
                    "cron.key environment variable."
            )
        return
    }

    var ref = admin.database().ref("games/")
    var now = Date.now()
    var cutoff = now - 3 * 60 * 60 * 1000 // 3 hours
    // var cutoff = now - 1 * 60 * 1000
    var oldItemsQuery = ref.orderByChild("timestamp").endAt(cutoff)
    return oldItemsQuery.once("value", snapshot => {
        // create a map with all children that need to be removed
        var updates = {}
        snapshot.forEach(child => {
            updates[child.key] = null
        })
        ref.update(updates)
        res.send(JSON.stringify(updates))
    })
})
