pipeline {
    agent any

    parameters {
        string(name: 'SHA', defaultValue: '', description: 'Genesis SHA - must be SHA, not ref!')
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
                    if (params.SHA == '') {
                        sh script: 'exit 1', label: 'missing genesis SHA'
                    }
                }
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh "./scripts/build-k8s-image.sh $params.SHA"
                }

                sh 'docker rmi -f $(docker images -q) || true'
            }
        }
    }
}
