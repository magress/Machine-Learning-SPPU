package test;

import java.io.*;

import weka.classifiers.bayes.NaiveBayes;
import weka.classifiers.Evaluation;
import weka.core.Instances;

public class testProg {

	public static void main(String[] args) throws Exception {
		
		BufferedReader trReader = new BufferedReader(new FileReader("c:/Users/NewSense/Documents/training.arff"));
		BufferedReader tsReader = new BufferedReader(new FileReader("c:/Users/NewSense/Documents/testing.arff"));
		
		Instances training = new Instances(trReader);
		Instances testing = new Instances(tsReader);
		
		trReader.close();
		tsReader.close();
		
		training.setClassIndex(training.numAttributes() - 1);
		testing.setClassIndex(testing.numAttributes() - 1);
		
		NaiveBayes nB = new NaiveBayes();
		nB.buildClassifier(training);
		System.out.println(nB);
		
		Evaluation eval = new Evaluation(testing);
		eval.evaluateModel(nB, testing);
		System.out.println(eval.toSummaryString("\nResults Testing\n", true));
		
		for(int i = 0; i < training.numClasses(); i++) {
			System.out.println("Class "+ i);
			System.out.println(eval.precision(i) + " " + eval.recall(i) + " " + eval.areaUnderROC(i));
		}
		
	}

}
