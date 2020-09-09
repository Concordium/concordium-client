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
                sshagent (credentials: ['6a7625a8-34f4-4c39-b0be-ed5b49aabc16']) {
                    sh './scripts/build-k8s-image.sh 829f3190a5eb938377e80fd2baf203e510a8908f'
                }

		sh 'docker rmi -f $(docker images -q) || true'
            }
        }
    }
}
