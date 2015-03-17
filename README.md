This library assists you in monitoring Consul k/v data & with proxying
data to Consul safely from the Internet.  The first use case is
receiving Github 'push' notifications when a repository is updated and
doing a git pull on all webservers to update content.  The [example](./example) app
shows this working.

You'll need to launch a small Consul cluster to try the example app.
It's easy to do with Docker.  Please use the following steps:

# Launch 4 containers of Docker w/ Consul running on each

    docker run -d --name node1 -h node1 progrium/consul -server -bootstrap-expect 3
    sleep 10
    JOIN_IP="$(docker inspect -f '{{.NetworkSettings.IPAddress}}' node1)"
    docker run -d --name node2 -h node2 progrium/consul -server -join $JOIN_IP
    docker run -d --name node3 -h node3 progrium/consul -server -join $JOIN_IP
    docker run -d -p 8400:8400 -p 8500:8500 -p 8600:53/udp --name node4 -h node4 progrium/consul -join $JOIN_IP
    sleep 10

## Try out all the interfaces of Consul

### Browse the [Web UI Interface](http://localhost:8500/ui/#/dc1/services/consul)

### Query Consul at the command line

    consul members

### Query the HTTP Interface

    curl 0.0.0.0:8500/v1/catalog/nodes

### Query the DNS Interface

    dig @0.0.0.0 -p 8600 node1.node.consul

# Setting up a Github Webhook

First we start \`ngrok\` to proxy HTTP requests from the Internet to
our local machine (most likely behind a firewall).  Then we'll start
the example web application to receive webhooks from Github.

Run the example application (in the root Github directory for
wai-middleware-consul).  We need to be in this directory so our
example app can find/read cabal & Github version status.

    ./.cabal-sandbox/bin/wai-middleware-consul-example

Try navigating to the example app dashboard page.  <http://0.0.0.0:8080>

Run ngrok to expose the example application to the Internet:

    ngrok 8080 ;# take note of this public web address for Github

Set up the webhook for the repo on Github. Point it to the ngrok web
address and add the path /github onto that URL ( eg,
<https://be2f75d.ngrok.com/github> ).  This is the specific route in
the example application for Github->Consul data updates.

Now that we have the application running and a proxy to the public
Internet established, we can receive webhook events.

Try pushing to the repository.  You should see notifications that an
event was received in the web application STDOUT log.  The event
should be pushed into Consul also.  The web application will be
listening to events from Consul & this will fire also.  You should
see it try to \`git pull\` the repository & you should see the changes
if you refresh the home page of the example app.

# Cleanup (kill & remove Consul containers)

    for n in $(seq 1 4); do
        docker kill node$n
        docker rm node$n    
    done
