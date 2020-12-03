pipeline {
    agent any

    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
            }
        }
        stage('build') {
            environment {
                GHC_VERSION = '8.8.4'
            }
            steps {
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh './scripts/distributables/linux-build-and-push-distributable-concordium-client.sh'
                }
            }
        }
    }
}
