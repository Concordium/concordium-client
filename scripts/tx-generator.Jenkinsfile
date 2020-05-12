pipeline {
    agent any
    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
            }
        }
        stage('build') {
            steps {
                sshagent (credentials: ['jenkins']) {
                    sh './scripts/build-k8s-image.sh 1881420eac611f4e75555ba5964b6b32f0ac616a'
                }
            }
        }
    }
}
