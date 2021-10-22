@Library('concordium-pipelines') _
pipeline {
    agent any
    environment {
        ecr_repo_domain = '192549843005.dkr.ecr.eu-west-1.amazonaws.com'
    }
    stages {
        stage('ecr-login') {
            steps {
                ecrLogin(env.ecr_repo_domain, 'eu-west-1')
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
                    // TODO Inline script.
                    sh "./scripts/build-k8s-image.sh $params.GENESIS_REF"
                }

                sh 'docker rmi -f $(docker images -q) || true'
            }
        }
    }
}
