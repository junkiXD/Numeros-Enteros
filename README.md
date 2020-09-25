# Numeros-Enteros
Problemas de numeros enteros orientada a objetos
Public Class NEnt
    Private n As Integer
    Public Sub NEnt()
        n = 0
    End Sub
    Public Sub Cargar(dato As Integer)
        n = dato
    End Sub
    Public Function Descargar() As Integer
        Return n
    End Function
    '==================PRACTICO 1===================
    '-- CLASES DE MOLLO
    '---FACTORIALES
    Public Function fact() As Double
        Dim i As Integer
        Dim f As Double
        If n > 0 Then
            f = 1
            For i = n To 1 Step -1
                f = f * i

            Next
        Else
            f = 1
        End If
        Return f
    End Function
    '----verifiacion de par
    Public Function VerifPar() As Boolean
        Dim re As Integer
        Dim b As Boolean                'VERIFICICACION de NE PARES
        re = n Mod 2
        If re = 0 Then
            b = True
        Else
            b = False
        End If
        Return b

    End Function
    '----- vERIFICACION DE MULTIPLOS DE n1 y n2 
    Public Function VerifMult(nu As NEnt) As Boolean
        Dim re As Integer           'Implemeto un n2 ... para el n2.cargar 
        Dim b As Boolean            'OJO EN los pasos
        re = n Mod nu.n
        If re = 0 Then
            b = True
        Else
            b = False
        End If
        Return b
    End Function
    '---VERIFICACION DE un NE si es primo 
    Public Function VerifPrim() As Boolean
        Dim c, re, i As Integer
        c = 0
        For i = 1 To n  'para i = 1 hasta n 
            re = n Mod i            'si decimos n=5 mod  de i=1
            If re = 0 Then              'si llea hasta re=0 es par
                c = c + 1
            End If
        Next
        Return c = 2

    End Function
    '==== Verificicaccion de NE primos (MAS recomendable para los practicos)
    Public Function VerifPrimos1() As Boolean
        Dim i As Integer
        Dim b As Boolean            'para buscar el par usamos dos variables
        If n > 1 Then           'PREGUNTAMOS si n mayor a 1, y damos valor a i = 1
            i = 1                   'este ejecutara un proceso sumando i +1 hsta que nos de 0 
            Do                      'si llega hasta el ejemplo 5 mod 5 = 0 quiere decir que es primo
                i = i + 1                   'pero si nos da antes del previsto quiere decir que no es par
            Loop Until n Mod i = 0              'porque topa con el primer numero repetior que no es par y nos dara 0
            b = n = i
        Else
            b = False
        End If
        Return b

    End Function

    '----Verificar si un NE pertenece a una serie exponencial
    Public Function VerifPertSer(b As NEnt) As Boolean
        Dim i, te As Integer            'ejemplo: 2^5=2,4,8,16,35
        i = -1                              'la pregunta si n1 el n= dato
        'aparece en el expo que se lleva elevado pn2 en este n2
        Do
            i = i + 1
            te = b.n ^ i
        Loop Until (te = n) Or (te > n)
        Return te = n
    End Function
    '---SUMAR DIGITOS --- N=1234   => ND=2  > N=12+34=X
    Public Function sumaDig(nd As Integer) As Integer
        Dim d, f, aux As Integer
        f = 0               'OBSERVA como se usan el mod y el div 
        aux = n             'en elmod sacamos lo que de 1234 mod 10^nd = 34
        While n > 0                 '
            d = n Mod 10 ^ nd
            n = n \ 10 ^ nd
            f = f + d
        End While
        n = aux
        Return f

    End Function
    ' ------------------COMIENZO DEL PRACTICO ..........................
    'PRACTICO 1:Verificar si un NE pertenece a la serie de fibonacci
    Public Function VerifPerSerFibo() As Boolean
        Dim a, b, fibo As Integer
        a = -1
        b = 1
        fibo = 0
        Do
            fibo = a + b
            a = b
            b = fibo
        Loop Until (fibo = n) Or (fibo > n)
        Return fibo = n
    End Function
    'PRACTICO 2 : Verificar si un numero pertenece a la serie regular dada la formula { t = vi +(i-1)*r}
    'donde i es una sucesion i=[ 1,2,3,4,5]
    Public Function Pract2() As Boolean
        Dim t, vi, r, i As Integer
        vi = 7
        r = 3
        Do
            t = vi + (i - 1) * r
            i = i + 1
        Loop Until (t = n) Or (t > n)
        Return t = n
    End Function
    'PRACTICO 3 : Accesar dígitos (de nd: # de dígitos) en sentido izquierda a derecha en una cadena/string
    'EJEMPLO : n =12345 y nd = 2 entonses textbox= "12,34,56"
    Public Function AccesDig(nd As Integer) As String
        Dim d, aux As Integer
        Dim s As String
        s = ""
        aux = n                 'ojo aqui con las referencias cadena/string
        While n > 0                     'AQUI el nd esta para el text box2 al lado de descargar n1
            d = n Mod 10 ^ nd
            n = n \ 10 ^ nd
            s = Str(d) + ";" + s        's=d+s 'El que esta compuesto es simple comodidad
        End While                       'No pasa nada como lo coloque en esa parte
        n = aux
        Return s

    End Function
    'PRACTICO 4:Filtrar (dejar pasar) dígitos impares de un NE de a nd dígitos, el resultado en un string.
    Public Function DejPasNEimp(nd As Integer) As String
        Dim d, aux As Integer    'OJO en estaoperacion es similar a la de accesar digitos y un if de par e impar
        Dim s As String
        s = ""
        aux = n
        While n > 0
            d = n Mod 10 ^ nd
            n = n \ 10 ^ nd
            If Not d Mod 2 = 0 Then 'OJO si  pide "Dejar pasar numero pares"entonses solo cambiamos el valor
                s = Str(d) + ";" + s    '"d n mod 2= 1"  => "d mod 2=0"  
            End If
        End While
        n = aux
        Return s
    End Function
    'PRACTICO 5: Filtrar (dejar pasar) dígitos primos de un NE de a nd dígitos, el resultado en un string:
    'EJEMPLO :Ejemplos: N=113417 y ND=2 => s=”17,11
    Public Function DejPasNEPrimos(nd As Integer) As String
        Dim aux As Integer
        Dim d As New NEnt       'para hacer este ejerciciodeclare un nueva clase
        Dim s As String         'con d para insertar la funcion de primo
        s = ""
        aux = n
        While aux > 0
            d.n = aux Mod 10 ^ nd
            aux = aux \ 10 ^ nd
            If d.VerifPrim Then
                s = Str(d.n) + ";" + s
            End If
        End While

        Return s
    End Function
    'PRACTICO 6: Acumular con dígitos de Fibonacci:
    'Ejemplo N=1234567 => F= 1/5! – 1/3! + 1/2! - 1/1! 
    Public Function AcuFibbo() As Double
        Dim aux As Integer
        Dim b As Boolean
        Dim f As Double
        Dim d As NEnt
        d = New NEnt
        aux = n
        f = 0
        b = True
        While (n > 0)
            d.n = n Mod 10
            n = n \ 10
            If d.VerifPerSerFibo Then
                If (b) Then
                    f = f + 1 / d.fact()
                Else
                    f = f - 1 / d.fact()
                End If
                b = Not b

            End If
        End While
        n = aux
        Return f

    End Function
    'PRACTICO 7 :Convertir un NE ( se supone en base 10) a base B. (2<=B>=9).
    'Ejemplo: N=21 a B=2 => R=10101
    'Ejemplo: N=36 a B=7 => R=51
    Public Function convertirEnBaseB(b As Integer) As Integer
        Dim s As String = ";" 'numero en base b
        Dim na, d As Integer
        na = n
        While na > 0
            d = na Mod b
            na = na \ b
            s = Str(d) + s
        End While
        ' el numero ya esta pero esta como una cadena
        'Return Integer.Parse(s) => otra forma de conv de string a int
        Return Val(s)
    End Function
    'Segunda forma
    Public Function ConvADec(b As Integer) As Integer
        Dim aux, d, p, a As Integer
        aux = n
        p = -1 : a = 0
        While n > 0         'La formula acumula en el 'a'

            d = n Mod b
            n = n \ b
            p = p + 1
            a = a + d * b ^ p
        End While
        n = aux
        Return a
    End Function

    'PRACTICO 8: Con dígitos primos de nd dígitos, acumular: (Ver el ejercicio N° 4 como ayuda)
    'Ejemplos: N=1234567 y ND=1 => F=√7+ 4^√5 + 6^ √3
    'N=132217 y ND=2 => F=2^√17  + 4^√13 
    Public Function AcuPrimos(nd As Integer) As Double
        Dim na, c As Integer
        Dim f, t As Double
        Dim d As NEnt
        d = New NEnt

        na = n
        c = 0
        f = 0
        While (n > 0)
            d.n = n Mod 10 ^ nd
            n = n \ 10
            If d.VerifPrim() Then
                c = c + 2
                t = d.n ^ (1 / c)
                f = f + t
            End If
        End While
        n = na
        Return f
    End Function





    'PRACTICO 9:Encontrar el digito mayor de ND dígitos:
    'Ejemplo: N=28371 y ND=1 => R=8
    'N=125423 y ND=2 => R=54
    'DIgito mayor ... primero hice una prueba para hallar el numero mayor descubri que es misma forma 
    Public Function DigMayor(nd As Integer) As Integer
        Dim d, dm, na As Integer
        na = n
        dm = n Mod 10 ^ nd
        n = n \ 10 ^ nd
        While n > 0
            d = n Mod 10 ^ nd
            n = n \ 10 ^ nd
            If d > dm Then
                dm = d
            End If
        End While
        n = na
        Return dm
    End Function
    Public Function EncontDigMayor(nd As Integer) As String 'esta es la verdadera forma para hallar por ND
        Dim d, dm, na As Integer
        na = n
        dm = n Mod 10 ^ nd      'prestar OJO a la forma en que se realiza omse pregunta una y cada deestos ejercicos
        n = n \ 10 ^ nd     'PRACTICAR despues como hallar
        While na > 0
            d = n Mod 10 ^ nd
            n = n \ 10 ^ nd
            If d > dm Then
                dm = d
            End If
        End While
        n = na
        Return dm
    End Function
    'PRACTICO 10:Encontrar la frecuencia de DIG de a nd dígitos de un NE.
    'jemplo. N=2462482 y nd=1 y DIG=2 => r=3
    'jemplo N = 335733 y nd=2 y DIG 33 => r=2

    '            FRECUENCIA DE UN NUMERO con un objeto como parametro
    Public Function FrecNumNdNE(nd As NEnt) As Integer
        Dim na, f, d, nnd As Integer
        na = n : f = 0
        nnd = nd.Ndigs()
        While n > 0
            d = n Mod 10 ^ nnd
            n = n \ 10 ^ nnd
            If nd.n = d Then
                f = f + 1
            End If
        End While
        n = na
        Return f
    End Function
    '=============================================================================== examen 2
    Public Sub elim1_dig(dig As NEnt)
        Dim e, d, nd As Integer
        Dim x As Boolean = True
        Dim f As NEnt = New NEnt
        nd = dig.CantDig
        While n > 0 And x = True
            d = n Mod 10 ^ nd
            n = n \ 10 ^ nd
            If d <> dig.n Then
                f.n = f.n + d * 10 ^ e
                e = e + nd
            Else
                x = False
            End If
        End While
        Unir2NE(f)

    End Sub
    Public Sub ordenar_num(nd As Integer)
        Dim f, e, d As Integer
        e = 0 : f = 0
        While n > 0
            d = num_mayor(nd)
            f = d * 10 ^ e + f
            e = e + nd
            ElimUnNum(d)
        End While
        n = f
    End Sub
    Public Function verif_primo()
        Dim x As Integer = 0
        For i = 1 To n
            If n Mod i = 0 Then
                x = x + 1
            End If
        Next
        Return x = 2
    End Function

    '' orenar sin eliminar
    Public Sub ordenarsin_elim(nd As Integer)
        Dim f As Integer
        Dim d As NEnt = New NEnt
        While n > 0
            d.n = num_mayor(nd)
            f = f * 10 ^ nd + d.n
            elim1_dig(d)
        End While
        n = f
    End Sub
    'Mantener orden
    Public Sub MantenerOrden()
        Dim ord, d, c As Integer
        ord = 0
        c = 1
        While n > 0
            d = n Mod 10
            n = n \ 10
            ord = (ord * c) + d
            c = c * 10
        End While
        n = ord
    End Sub

    Public Function DigMenor() As Integer
        Dim aux, men, d As Integer
        aux = n
        men = n Mod 10
        n = n \ 10
        While n > 0
            d = n Mod 10
            n = n \ 10
            If d < men Then
                men = d
            End If
        End While
        n = aux
        Return men
    End Function
    'Dig menor, POR ND  para tomar digitos
    Public Function DigMenorAND(nd As Integer) As Integer
        Dim aux, men, d As Integer
        aux = n
        men = n Mod 10 ^ nd
        n = n \ 10 ^ nd
        While n > 0
            d = n Mod 10 ^ nd
            n = n \ 10 ^ nd
            If d < men Then
                men = d
            End If
        End While
        n = aux
        Return men
    End Function
    'Dig Mayor
    Public Function DigMayor() As Integer
        Dim aux, men, d As Integer
        aux = n
        men = n Mod 10
        n = n \ 10
        While n > 0
            d = n Mod 10
            n = n \ 10
            If d > men Then
                men = d
            End If
        End While
        n = aux
        Return men
    End Function
    'cantidad de digistosPARA EJERCICI 1 otro metodo nDigs()

    'Dig Mayor A ND digitos para 
    Public Function DigMayorNDDigit(nd As Integer) As Integer
        Dim aux, men, d As Integer
        aux = n
        men = n Mod 10 ^ nd
        n = n \ 10 ^ nd
        While n > 0
            d = n Mod 10 ^ nd
            n = n \ 10 ^ nd
            If d > men Then
                men = d
            End If
        End While
        n = aux
        Return men
    End Function

    'FRECUENCIA A nd entero como parametro
    Public Function Frecuencia(dig As Integer) As Integer
        Dim d, c As Integer
        Dim aux As Integer = n
        c = 0
        While n > 0
            d = n Mod 10
            n = n \ 10
            If dig = d Then
                c = c + 1
            End If
        End While
        n = aux
        Return c
    End Function
    'Eliminar a nd
    Public Sub ELIMINAR_NUMERO(dig As Integer)
        Dim d, f, e As Integer
        Dim nd As NEnt = New NEnt
        nd.n = dig : e = 0 : f = 0
        While n > 0
            d = n Mod 10 ^ nd.Ndigs
            n = n \ 10 ^ nd.Ndigs
            If d <> dig Then
                f = d * 10 ^ e + f
                e = e + nd.Ndigs


            End If
        End While
        n = f
    End Sub


    'FRECUENCIA CON UN OBJETO EN PARAMETRO


    '            FRECUENCIA DE UN NUMERO con un objeto como parametro


    '---------UNIR 2 NE en uno solo
    Public Function Unir2NE(nn As NEnt) As Integer
        Return n * 10 ^ nn.Ndigs() + nn.n
    End Function
    'SI EXISTE UN DIGITO EN UN NE OJO solo retorna un dig
    '------ECONTRAR dig menores
    Public Function BusDigMen() As Integer
        Dim ra, aux, d As Integer
        aux = n
        ra = n Mod 10
        n = n \ 10
        While n > 0
            d = n Mod 10
            n = n \ 10
            If d < ra Then
                ra = d
            End If
        End While
        n = aux
        Return ra
    End Function
    'ELIMINAR UN ne PARA EL 1
    Public Sub ElimUnNum(nd As Integer)
        Dim b As Boolean
        Dim d, e As Integer
        Dim nn As NEnt = New NEnt
        d = 0 : e = 0 : b = False
        While (n > 0) And (b = False)
            d = n Mod 10 : n = n \ 10
            If d = nd Then
                b = True
            Else
                nn.n = nn.n + d * 10 ^ e
                e = e + 1
            End If
        End While
        Unir2NErEsteNECESARIO(nn)
    End Sub

    '1. Cortar / Eliminar dígitos repetidos.
    'Ejemplo. N=2324454 => N=35 (So quedan los dígitos únicos)
    Public Sub ElimDigRep()
        Dim d, p As Integer
        Dim nn As NEnt = New NEnt
        ' Dim d As NEnt = New NEnt          n=12869  n=869
        Dim s As NEnt = New NEnt
        p = -1
        nn.n = 0
        s.n = n
        While n > 0
            d = n Mod 10
            n = n \ 10
            If (s.Frecuencia(d)) = 1 Then

                p = p + 1
                nn.n = nn.n + d * 10 ^ p

            End If
        End While
        nn.OrdDescenden()
        n = nn.n
    End Sub
    ''2. Segmentar en dígitos repetidos y únicos.
    'Ejemplo. N=1322118 => N=1221138 Mantener orden
    Public Sub SegmRepyNREP()
        Dim c1, c2 As Integer
        Dim r As NEnt = New NEnt
        Dim nr As NEnt = New NEnt
        Dim d As NEnt = New NEnt
        Dim s As NEnt = New NEnt
        c1 = 0 : c2 = 0
        s.n = n
        While n > 0
            d.n = n Mod 10 : n = n \ 10
            s.n = n
            If Frecuencia(d.n) > 1 Then
                nr.n = nr.n + d.n * 10 ^ c2
                c2 = c2 + 1
            Else
                r.n = r.n + d.n * 10 ^ c1
                c1 = c1 + 1
            End If
        End While
        nr.OrdAsc()


        r.Unir2NE(nr)
        n = r.n

    End Sub
    'SEGMENTAR METODO DE USO 2
    Public Sub SegDigRepetidosYunicos()
        Dim d As NEnt = New NEnt
        Dim s As NEnt = New NEnt
        Dim rep As NEnt = New NEnt
        Dim Nrep As NEnt = New NEnt
        Dim ac, ac2 As Integer
        ac = 1 : ac2 = 1
        s.n = n
        While n > 0
            d.n = n Mod 10
            n = n \ 10
            If s.Frecuencia(d.n) > 1 Then
                rep.n = (d.n * ac) + rep.n
                ac = ac * 10
            Else
                Nrep.n = Nrep.n + (d.n * ac2)
                ac2 = ac2 * 10
            End If

        End While
        Nrep.OrdAsc()

        n = (rep.n * ac2) + Nrep.n
    End Sub

    '3. Ordenar de nd dígitos..
    'Ejemplo: sea N = 2845 y nd=1 => N= 2458
    'N=4321 y nd=2 = >N=2143
    'N=7654321 y nd=3 = > N=7321654
    Public Sub OrdDigts(nd As Integer)
        Dim nu As NEnt = New NEnt
        Dim lo As Integer

        Dim d As NEnt = New NEnt

        While n > 0
            d.n = n Mod 10 ^ nd
            n = n \ 10 ^ nd
            nu.n = nu.n * 10 ^ nd + d.n

        End While
       
        n = nu.n
    End Sub
    '===============contar digitos
    '---------------------Para contar cuantas digitos tiene un NE
    Public Function Ndigs() As Integer
        Dim aux, nd As Integer
        aux = n
        nd = 0
        While n > 0
            n = n \ 10
            nd = nd + 1
        End While
        n = aux
        Return nd
    End Function
    Public Function NdigsNEnd(nd As Integer) As Integer
        Dim aux, lo As Integer
        aux = n
        lo = 0
        While n > 0
            n = n \ 10 ^ nd
            lo = lo + 1
        End While
        n = aux
        Return lo
    End Function
    '==============================UNION
    'UNIR 2 NUMEROS PARA EJERCCIO 1
    Public Sub Unir2NErEsteNECESARIO(nu As NEnt)
        n = n * 10 ^ (nu.Ndigs) + nu.n
    End Sub
    Public Function Unir2NEPORnd(nd As Integer)

    End Function
    '4. Cambiar un número (digito (s)) por otro de un NE.
    'N=213452 cambiar NA1{N=34 por NA 2{N=10 => N=211052
    'Nota.- NA1 y NA2 son objetos.
    Public Sub CambioNE(na1 As NEnt, na2 As NEnt)
        Dim d, nd, p As Integer
        Dim na As NEnt = New NEnt
        Dim b As Boolean = True
        p = -1
        nd = na1.Ndigs
        While (n > 0) And (b = True)
            d = n Mod 10 ^ nd
            n = n \ 10

            If Not (d = na1.n) Then
                p = p + 1
                d = d Mod 10
                na.n = na.n + d * 10 ^ p

            Else
                b = False
                n = n \ 10 ^ (nd - 1)
            End If


        End While
        Me.Unir2NErEsteNECESARIO(na2)
        Me.Unir2NErEsteNECESARIO(na)
    End Sub
    'OTRO metodo Cambio
    Public Sub CamioNE2(na2 As NEnt, na3 As NEnt)
        Dim s As NEnt = New NEnt
        Dim d As NEnt = New NEnt
        Dim nd, cam As Integer
        nd = na2.Ndigs
        While n > 0
            d.n = n Mod 10 ^ nd
            n = n \ 10
            If d.n = na2.n Then
                na2.n = na3.n
            End If
        End While
    End Sub

    'Numeros pares
    Public Function PARES() As Boolean

        Dim b As Boolean
        If n Mod 2 = 0 Then
            b = True
        Else
            b = False
        End If
        Return b
    End Function
    '== verificar pares

    '             VERIFICAR SI HAY SOLO UN PAR
    Public Function VerificUnPar() As Boolean
        Dim na, fd, d As Integer
        d = 0 : na = n = fd = 0
        While n > 0
            d = n Mod 10 : n = n \ 10
            If Frecuencia(d) = 2 Then
                fd = fd + 1
            End If
        End While
        n = na
        Return fd = 2
    End Function
    '    VERIFICAR SI UN NUMERO ES PRIMO
    Public Function Digpar() As Integer
        Dim d, par, aux As Integer
        aux = n
        While n > 0
            d = n Mod 10
            n = n \ 10
            If d Mod 2 = 0 Then
                par = d
            End If
        End While
        n = aux
        Return par
    End Function
    ' VERIFICAR DIG IMPARES
    Public Function DigImpar() As Integer
        Dim d, Impar, aux As Integer
        aux = n
        While n > 0
            d = n Mod 10
            n = n \ 10
            If d Mod 2 = 1 Then
                Impar = d
            End If
        End While
        n = aux
        Return Impar
    End Function
    '5. Intercalar dígitos de pares e impares de nd dígitos
    'Ejemplo: N=43544 nd=1 => N=45434
    'N=57344 nd=2 => N=447305
    '=================
    Public Sub INTER_PAR(nd As Integer)
        Dim dig As Integer

        Dim n1 As NEnt = New NEnt
        Dim n2 As NEnt = New NEnt

        Dim aux As NEnt = New NEnt
        Dim pos = 1
        Dim pos2 = 1
        nd = 10 ^ nd
        aux.n = n
        n1.n = 0 : n2.n = 0
        While n > 0
            dig = n Mod nd
            n = n \ nd
            If dig Mod 2 = 0 Then
                n1.n = n1.n + dig * pos ' digitos pares
                pos = pos * nd
            Else
                n2.n = n2.n + dig * pos2 'dig no repetidos
                pos2 = pos2 * nd
            End If
        End While

        While n1.n > 0 Or n2.n > 0

            If n1.n > 0 Then
                dig = n1.n Mod nd
                n1.n = n1.n \ nd

                n = n * nd + dig 'resultado
            End If
            If n2.n > 0 Then
                dig = n2.n Mod nd
                n2.n = n2.n \ nd

                n = n * nd + dig 'RESULTADO
            End If
        End While
    End Sub


    ' Verificar si un numero es Primo
    Function verificar_primo() As Boolean
        Dim b As Boolean = False
        Dim c As Integer = 1

        If n > 1 Then
            Do
                c = c + 1
            Loop Until (n Mod c = 0) Or (c > n)
            If c = n Then
                b = True
            End If
        Else
            If n = 1 Then
                b = True
            End If
        End If
        Return b
    End Function


    Public Sub IntercalrParyNpar()
        Dim b As Boolean = True
        Dim npar As NEnt = New NEnt
        Dim par As NEnt = New NEnt
        Dim nu As Integer
        Dim p, p2 As Integer
        p2 = 1 : p = 1
        nu = 0
        While n > 0

            If b Then
                par.n = Digpar()
                nu = nu * 10 + par.n


                ElimUnNum(par.n)
            Else
                npar.n = DigImpar()

                nu = (nu * 10) + npar.n

                ElimUnNum(npar.n)

            End If
            b = Not b
        End While


        n = nu
    End Sub
    '=============================
    'Hallar orde ascendente
    Public Sub OrdAsc()
        Dim nn, d As Integer

        While n > 0
            d = DigMenor()
            nn = nn * 10 + d
            ElimUnNum(d)
        End While
        n = nn
    End Sub
    ' Orden asc a nd 
    Public Sub OrdAscaND(nd As Integer)
        Dim f, e, d As Integer
        e = 0 : f = 0
        While n > 0
            d = DigMenorAND(nd)
            f = f * 10 ^ nd + d
            ELIMINAR_NUMERO(d)
        End While
        n = f

    End Sub
    'ORenar descendente 
    ''' <summary>
    ''' Ojo como se desarrolla
    ''' 
    ''' </summary>
    Public Sub OrdDescenden()
        Dim nn, d As Integer

        While n > 0
            d = DigMayor()
            nn = nn * 10 + d
            ElimUnNum(d)
        End While
        n = nn
    End Sub
    'Orden descendente a nd
    Public Sub OrdDescaND(nd As Integer)

        Dim f, e, d As Integer
        e = 0 : f = 0
        While n > 0
            d = DigMayorNDDigit(nd)
            f = f * 10 ^ nd + d
            ELIMINAR_NUMERO(d)
        End While
        n = f
    End Sub
    'ORDENAR 
    Public Sub ORDEASCde3Unidos(n2 As NEnt, n3 As NEnt)
        Dim a As NEnt = New NEnt
        Dim b As NEnt = New NEnt
        Dim c As NEnt = New NEnt
        Dim t, x As Integer
        t = 0
        a.n = n

        b.n = n2.n

        c.n = n3.n
        'a=12  : b=3 :  c= 9
        For x = 1 To 3
            If a.n < b.n Then

                t = a.n

                a.n = b.n

                b.n = t
            End If

            If b.n < c.n Then
                t = b.n

                b.n = c.n

                c.n = t

            End If
        Next x
        a.Unir2NErEsteNECESARIO(b)
        a.Unir2NErEsteNECESARIO(c)
        n = a.n

    End Sub





    '6. Unir 3 números enteros en orden ascendente.
    'Ejemplo: N=34 N2{N=123 y N3{N=7 => un=734123
    'Ejemplo: N=3 N2{N=12 y N3{N=7 => un=3712
    '''

    '--------------------------------Pregunta 2---------------------------
    'PREGUNTAMOS POR CADA OBJETO)
    Function Menor3(N2 As NEnt, N3 As NEnt)
        Dim men As NEnt = New NEnt
        men.n = n
        If N2.n > men.n Then men.n = N2.n
        If N3.n > men.n Then men.n = N3.n
        Return men.n
    End Function
    Function Menor4(N2 As NEnt, N3 As NEnt, N4 As NEnt)
        Dim men As NEnt = New NEnt
        men.n = n
        If N2.n > men.n Then men.n = N2.n
        If N3.n > men.n Then men.n = N3.n
        If N4.n > men.n Then men.n = N4.n
        Return men.n
    End Function
    Function SegMenor4(N2 As NEnt, N3 As NEnt, N4 As NEnt)
        Dim men As NEnt = New NEnt
        men.n = Menor4(N2, N3, N4)
        If men.n = n Then Return N2.Menor3(N3, N4)
        If men.n = N2.n Then Return Menor3(N3, N4)
        If men.n = N3.n Then Return Menor3(N2, N4)
        Return Menor3(N2, N3)
    End Function
    Function Mayor3(N2 As NEnt, N3 As NEnt)
        Dim men As NEnt = New NEnt
        men.n = n
        If N2.n < men.n Then men.n = N2.n
        If N3.n < men.n Then men.n = N3.n
        Return men.n
    End Function
    Function Mayor4(N2 As NEnt, N3 As NEnt, N4 As NEnt)
        Dim men As NEnt = New NEnt
        men.n = n
        If N2.n < men.n Then men.n = N2.n
        If N3.n < men.n Then men.n = N3.n
        If N4.n < men.n Then men.n = N4.n
        Return men.n
    End Function
    Function SegMayor4(N2 As NEnt, N3 As NEnt, N4 As NEnt)
        Dim men As NEnt = New NEnt
        men.n = Mayor4(N2, N3, N4)
        If men.n = n Then Return N2.Mayor3(N3, N4)
        If men.n = N2.n Then Return Mayor3(N3, N4)
        If men.n = N3.n Then Return Mayor3(N2, N4)
        Return Mayor3(N2, N3)
    End Function
    Function CantDig()
        Return Math.Truncate(Math.Log10(n)) + 1
    End Function
    Sub Unir(N2 As NEnt)
        n = n * 10 ^ N2.CantDig() + N2.n
    End Sub
    'Hallar de forma ascendentte los 4 objetos 

    Public Sub Unir4Desc(N2 As NEnt, N3 As NEnt, N4 As NEnt)
        Dim men As NEnt = New NEnt
        Dim Segmen As NEnt = New NEnt
        Dim SegMay As NEnt = New NEnt
        Dim May As NEnt = New NEnt

        men.n = Menor4(N2, N3, N4)
        Segmen.n = SegMenor4(N2, N3, N4)
        SegMay.n = SegMayor4(N2, N3, N4)
        May.n = Mayor4(N2, N3, N4)

        May.Unir(SegMay)
        May.Unir(Segmen)
        May.Unir(men)

        n = May.n
    End Sub
    '===================================================================  ' 'prueba de codigo 2


    '7. Dividir un NE en 2 números cuando los dígitos sean iguales.
    'Ejemplo: N=2344567 = > N1{N=234 N2{N=4567
    'Ejemplo: N=25667 = > N1{N=256 N2{N=67

    '7. Dividir un NE en 2 números cuando los dígitos sean iguales.
    'Ejemplo: N=2344567 = > N1{N=234 N2{N=4567
    'Ejemplo: N=25667 = > N1{N=256 N2{N=67
    Public Function DivEn2NE(n2 As NEnt)
        Dim dig1, dig2 As Integer

        Dim pos As Integer = 1
        Dim b As Boolean = True
        n2.n = 0
        While n > 9 And b
            dig1 = n Mod 10
            n = n \ 10
            dig2 = n Mod 10
            If dig1 <> dig2 Then
                n2.n = n2.n + dig1 * pos
                pos = pos * 10
            Else
                n2.n = n2.n + dig1 * pos
                b = False

            End If

        End While
        Return n
    End Function
    ' Pasos parael8
    Public Function num_mayor(nd As Integer)
        Dim d, may, NA As Integer
        NA = n : may = NA Mod 10 ^ nd
        While NA > 0
            d = NA Mod 10 ^ nd
            NA = NA \ 10 ^ nd
            If d > may Then
                may = d
            End If
        End While
        Return may
    End Function
    Public Function num_menor(nd As Integer)
        Dim na, men, d As Integer
        na = n : men = na Mod 10 ^ nd
        While na > 0
            d = na Mod 10 ^ nd
            na = na \ 10 ^ nd
            If d < men Then
                men = d
            End If
        End While
        Return men
    End Function
    '== FRECUENCIA A ND INTEGER  como parametro
    Public Function frec_num(nu As Integer)
        Dim d, na, f, nd As Integer
        Dim num As New NEnt
        na = n : num.n = nu : f = 0 : nd = num.CantDig
        While na > 0
            d = na Mod 10 ^ nd
            na = na \ 10 ^ nd
            If nu = d Then
                f = f + 1
            End If
        End While
        Return f
    End Function
    '==================

    '8. Verificar si es TRICA. (3 dígitos iguales y los otros distintos)
    'Ejemplo: N=23252 => TRUE (2 HACE TRICA)
    Public Function verif_trica() As Boolean
        Dim na, d As Integer
        na = n
        Dim x As Boolean = False
        If Not (verif_full()) Then
            While na > 0 And x = False
                d = na Mod 10
                na = na \ 10
                If frec_num(d) = 3 Then
                    x = True
                End If
            End While
        End If
        Return x
    End Function
    '9. Verificar si es 2 PARES. (Debe existir exactamente 2 pares)
    'Ejemplo: N=32443 => TRUE (3 UN PAR Y 4 OTRO PAR)

    '           VERIFICAR SI HAY DOS PARES

    Public Function verif_2pares()
        Dim f, na, d As Integer
        na = n
        While na > 0
            d = na Mod 10
            na = na \ 10
            If frec_num(d) = 2 Then
                f = f + 1
            End If
        End While
        Return f = 4
    End Function
    '10. Verificar si es full (3 dígitos iguales y los otros 2 iguales también, pero distintos a
    'los 3 iguales)
    'Ejemplo: N=43443 => TRUE (4 tres veces y 3 dos veces)
    'Verificar full
    Public Function verif_full() As Boolean
        Return (frec_num(DigMayor()) = 3) And (frec_num(DigMenor()) = 2) Or
            (frec_num(DigMayor()) = 2) And (frec_num(DigMenor()) = 3)
    End Function

End Class

