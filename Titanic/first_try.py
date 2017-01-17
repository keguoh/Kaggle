import numpy as np
import pandas as pd

#Print you can execute arbitrary python code
titanic = pd.read_csv("../input/train.csv", dtype={"Age": np.float64}, )
titanic_test = pd.read_csv("../input/test.csv", dtype={"Age": np.float64}, )

# The titanic variable is available here.
titanic["Age"] = titanic["Age"].fillna(titanic["Age"].median())

# Find all the unique genders -- the column appears to contain only male and female.
print(titanic["Sex"].unique())

# Replace all the occurences of male with the number 0.
titanic.loc[titanic["Sex"] == "male", "Sex"] = 0

# Replace all the occurences of female with the number 1.
titanic.loc[titanic["Sex"] == "female", "Sex"] = 1

# Find all the unique values for "Embarked".
print(titanic["Embarked"].unique())

titanic.loc[titanic["Embarked"] == "S", "Embarked"] = 0
titanic["Embarked"] = titanic["Embarked"].fillna(0)
titanic.loc[titanic["Embarked"] == "C", "Embarked"] = 1
titanic.loc[titanic["Embarked"] == "Q", "Embarked"] = 2



titanic_test["Age"] = titanic_test["Age"].fillna(titanic["Age"].median())

# Replace all the occurences of male with the number 0.
titanic_test.loc[titanic_test["Sex"] == "male", "Sex"] = 0
titanic_test.loc[titanic_test["Sex"] == "female", "Sex"] = 1

titanic_test.loc[titanic_test["Embarked"] == "S", "Embarked"] = 0
titanic_test["Embarked"] = titanic_test["Embarked"].fillna(0)
titanic_test.loc[titanic_test["Embarked"] == "C", "Embarked"] = 1
titanic_test.loc[titanic_test["Embarked"] == "Q", "Embarked"] = 2

titanic_test["Fare"] = titanic_test["Fare"].fillna(titanic_test["Fare"].median())

# The columns we'll use to predict the target
predictors = ["Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked"]

from sklearn import cross_validation
# Import the linear regression class
from sklearn.linear_model import LinearRegression, LogisticRegression

# Initialize the algorithm class
alg = LogisticRegression(random_state=1)

# Train the algorithm using all the training data
alg.fit(titanic[predictors], titanic["Survived"])

# Make predictions using the test set.
predictions = alg.predict(titanic_test[predictors])

# Create a new dataframe with only the columns Kaggle wants from the dataset.
submission = pd.DataFrame({
        "PassengerId": titanic_test["PassengerId"],
        "Survived": predictions
    })
    

#Any files you save will be available in the output tab below
submission.to_csv('submission.csv', index=False)
