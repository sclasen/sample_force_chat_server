#Sample VMForce Lift Chat Server

This is a Scala/Lift example app that deploys to VMForce and uses the VMForce JPA provider to persist data.

##Build and Deploy

You can build and deploy with maven. You will need an active profile with the following properties set.

 <profile>
     <id>vmforce-dev</id>
     <properties>
        <force.apiVersion>20.0</force.apiVersion>
        <force.endPoint>your endpoint</force.endPoint>
        <force.userName>your username</force.userName>
        <force.password>your password</force.password>
     </properties>
  </profile>
