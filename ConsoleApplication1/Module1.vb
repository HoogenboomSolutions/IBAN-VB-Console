Module MyIBAN

    Dim Banken = New Dictionary(Of String, String) From
    {
        {"00", "INGB"},
        {"10", "RABO"},
        {"11", "RABO"},
        {"12", "RABO"},
        {"13", "RABO"},
        {"14", "RABO"},
        {"15", "RABO"},
        {"16", "RABO"},
        {"17", "RABO"},
        {"18", "RABO"},
        {"19", "TRIO"},
        {"21", "ABNA"},
        {"22", "ABNA"},
        {"23", "ABNA"},
        {"24", "ABNA"},
        {"25", "ABNA"},
        {"26", "FVLB"},
        {"28", "BNGH"},
        {"29", "FRBK"},
        {"30", "RABO"},
        {"31", "RABO"},
        {"32", "RABO"},
        {"33", "RABO"},
        {"34", "RABO"},
        {"35", "RABO"},
        {"36", "RABO"},
        {"37", "RABO"},
        {"38", "RABO"},
        {"39", "TRIO"},
        {"40", "ABNA"},
        {"41", "ABNA"},
        {"42", "ABNA"},
        {"43", "ABNA"},
        {"44", "ABNA"},
        {"45", "ABNA"},
        {"46", "ABNA"},
        {"47", "ABNA"},
        {"48", "ABNA"},
        {"49", "ABNA"},
        {"50", "ABNA"},
        {"51", "ABNA"},
        {"52", "ABNA"},
        {"53", "ABNA"},
        {"54", "ABNA"},
        {"55", "ABNA"},
        {"56", "ABNA"},
        {"57", "ABNA"},
        {"58", "ABNA"},
        {"59", "ABNA"},
        {"61", "ABNA"},
        {"62", "ABNA"},
        {"64", "ABNA"},
        {"65", "INGB"},
        {"66", "INGB"},
        {"67", "INGB"},
        {"68", "INGB"},
        {"69", "INGB"},
        {"71", "ABNA"},
        {"72", "AEGO"},
        {"73", "AEGO"},
        {"74", "AEGO"},
        {"75", "INGB"},
        {"76", "ABNA"},
        {"77", "AEGO"},
        {"79", "INGB"},
        {"80", "ABNA"},
        {"82", "SNSB"},
        {"83", "ABNA"},
        {"84", "ABNA"},
        {"85", "ASNB"},
        {"86", "ABNA"},
        {"87", "ABNA"},
        {"88", "ABNA"},
        {"89", "ABNA"},
        {"90", "SNSB"},
        {"91", "SNSB"},
        {"92", "SNSB"},
        {"93", "SNSB"},
        {"94", "ABNA"},
        {"95", "SNSB"},
        {"96", "SNSB"},
        {"98", "ABNA"}
    }

    Function isBankNummer(Rekening As String) As Boolean

        Dim i As Integer
        Dim iBank As Integer

        If Len(Rekening) < 9 Then
            Return True
        ElseIf Len(Rekening) > 10 Then
            Return False
        Else
            iBank = 0
            For i = 0 To 8
                iBank = iBank + (9 - i) * (Asc(Rekening(i)) - Asc("0"))
            Next i
        End If

        If iBank Mod 11 <> 0 Then
            Console.WriteLine("Rekening " & Rekening & " is geen valide banknummer.")
        End If

        isBankNummer = (iBank Mod 11 = 0)

    End Function

    Function GetBank(Rekening As String) As String

        Dim Value As String
        Dim BankCode As String

        Value = ""
        BankCode = Right(Left(Rekening, 3), 2)
        If Not Banken.TryGetValue(BankCode, Value) Then
            Console.WriteLine("Rekening " & Rekening & " heeft geen valide Nederlandse bank.")
        End If

        GetBank = Value

    End Function


    Function IBAN(Rekening As String) As String
        Dim Bank As String
        Dim Land As String
        Dim i As Decimal
        Dim e As Decimal
        Dim ControleGetal As String
        Dim BankGetal As String
        Dim LandGetal As String

        Land = "NL"
        IBAN = ""

        BankGetal = ""
        LandGetal = ""
        If isBankNummer(Rekening) Then
            If Len(Rekening) < 10 Then
                For i = 1 To 10 - Len(Rekening)
                    Rekening = "0" & Rekening
                Next i
            End If
            Bank = GetBank(Rekening)
            If Bank <> "" Then
                For j = 0 To 3
                    BankGetal = BankGetal & (Asc(UCase(Bank(j))) - 55)
                Next j
                For j = 0 To 1
                    LandGetal = LandGetal & (Asc(UCase(Land(j))) - 55)
                Next j
                e = CDec(BankGetal & Rekening & Landgetal & "00")
                i = 98D - (e Mod 97D)
                If i >= 10 Then
                    ControleGetal = CStr(i)
                Else
                    ControleGetal = "0" & CStr(i)
                End If
                IBAN = Land & ControleGetal & Bank & Rekening
            End If
        End If

    End Function
    Sub Main()

        Dim RekNr As String

        RekNr = "449304272"             'Onze En/Of         ==> 16
        Console.WriteLine(IBAN(RekNr))
        RekNr = "1029106"               'Postgiro bob       ==> 37
        Console.WriteLine(IBAN(RekNr))
        RekNr = "198448104"             'Triodos Hélène     ==> 52
        Console.WriteLine(IBAN(RekNr))
        RekNr = "850452821"             'ASN Hélène/bob     ==> 33
        Console.WriteLine(IBAN(RekNr))
        RekNr = "1026564"               'Postgiro Cedric    ==> 92
        Console.WriteLine(IBAN(RekNr))
        RekNr = "9199413"               'Postgiro Megan     ==> 21
        Console.WriteLine(IBAN(RekNr))
        RekNr = "198354274"             'Triodos Julian     ==> 16
        Console.WriteLine(IBAN(RekNr))
        RekNr = "693553309"             'Fout banknummer
        Console.WriteLine(IBAN(RekNr))
        RekNr = "201234564"             'Geen bank, wel valide banknummer
        Console.WriteLine(IBAN(RekNr))

        Console.ReadLine()

    End Sub
End Module