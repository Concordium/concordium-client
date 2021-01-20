pipeline {
    agent any

    parameters {
        string(name: 'GENESIS_REF', defaultValue: '', description: 'Genesis ref - must be branch or tag, not SHA!')
    }

    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
            }
        }
        stage('build') {
            steps {
                script {
                    if (params.GENESIS_REF == '') {
                        sh script: 'exit 1', label: 'missing genesis ref'
                    }
                }
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh "./scripts/build-k8s-image.sh $params.GENESIS_REF"
                }

                sh 'docker rmi -f $(docker images -q) || true'
            }
        }
    }
}
