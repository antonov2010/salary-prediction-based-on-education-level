# Definición de las variables de la ENE
variables = {
    "Localidad (LOC)": "Cualitativa",
    "Ciudad autorrepresentada (CD_A)": "Cualitativa",
    "Entidad (ENT)": "Cualitativa",
    "Condición de residencia (C_RES)": "Cualitativa",
    "Sexo (SEX)": "Cualitativa",
    "Edad (EDA)": "Cuantitativa",
    "Año de nacimiento (NAC_ANIO)": "Cuantitativa",
    "¿Sabe leer y escribir un recado? (CS_P12)": "Cualitativa",
    "¿Hasta qué grado aprobó... en la escuela (nivel escolar)? (CS_P13_1)": "Mixta",
    "¿Hasta qué año aprobó ... en la escuela? (CS_P13_2)": "Cuantitativa",
    "Clave de la carrera (CS_P14_C)": "Cuantitativa",
    "¿Qué estudios le pidieron a ... para ingresar a esta carrera? (CS_P15)": "Cualitativa",
    "¿...Terminó los estudios o materias de esta carrera? (CS_P16)": "Cualitativa",
    "¿...Asiste actualmente a la escuela? (CS_P17)": "Cualitativa",
    "En total cuántas hijas e hijos que nacieron vivos ha tenido (N_HIJ)": "Cuantitativa",
    "Estado conyugal (E_CON)": "Cualitativa",
    "¿A qué estado de la república o país se fue ... ? (CS_AD_DES)": "Cualitativa",
    "¿Cuál es el motivo principal por el que llegó ...? (CS_NR_MOT)": "Cualitativa",
    "¿De qué estado de la República Mexicana o país vino ...? (CS_NR_ORI)": "Cualitativa",
    "Zona salarial (ZONA)": "Cualitativa",
    "Salario mínimo mensual (SALARIO)": "Cuantitativa",
    "Ponderador (FAC)": "Cuantitativa",
    "Años de escolaridad (ANIOS_ESC)": "Cuantitativa",
    "Horas trabajadas en la semana (HRSOCUP)": "Cuantitativa",
    "Ingreso mensual (INGOCUP)": "Cuantitativa",
    "Promedio de ingreso por hora trabajada (ING_X_HRS)": "Cuantitativa",
    "Total de trabajos (T_TRA)": "Cuantitativa",
    "Horas trabajadas en el trabajo principal (HRS_TRAB_PPAL)": "Cuantitativa",
    "Ingreso del trabajo principal (ING_TRAB_PPAL)": "Cuantitativa",
    "Clasificación de la población ocupada que trabajan menos de 35 horas más de 35 horas y de 48 horas (TCCO)": "Cualitativa",
    "Población ocupada que trabaja más de 48 horas y gana menos un salario mínimo (MA48ME1SM)": "Cualitativa",
    "Clasificación de actividades económicas con base al sistema de clasificación Industrial de América del Norte (SCIAN)": "Cualitativa",
    "Trabajadores transfronterizos del trabajo principal (TRANS_PPAL)": "Cualitativa",
}

# Generar la salida en CSV
import csv
import re

def procesar_variable(variable_original):
  last_index = variable_original.rindex("(")
  nemonico = variable_original[last_index:].replace("(", "").replace(")", "").replace('"', "")
  descripcion = variable_original[:last_index-1]
  resultado = [nemonico, descripcion]
  return resultado


with open('ene_variables.csv', 'w', newline='') as csvfile:

  writer = csv.writer(csvfile)
  writer.writerow(["Variable", "Descripción", "Tipo de variable"])
  for variable in variables:
    variable_procesada = procesar_variable(variable)
    variable_procesada.append(variables[variable])
    writer.writerow(variable_procesada)

print("Archivo CSV generado exitosamente: ene_variables.csv")