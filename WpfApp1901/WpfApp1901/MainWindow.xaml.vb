Imports System.Text.RegularExpressions

Class MainWindow
    Private Property n As Long  'تعداد شی
    Private Property m As Long  'تعداد جعبه
    Private Property K As Double  'اندازه جعبه
    Private Property Obj As Double() 'اندازه اشیاء

    Private Sub ButSave_Click()

        Me.txtResult.Text = String.Empty
        If Me.Validation = False Then
            Exit Sub
        End If


        If IsNothing(Me.Obj) Then
            Exit Sub
        End If

        'GetStartIndex
        Dim StartIndex As Long = GetStartIndex()


        For j As Long = StartIndex To Me.Obj.Length - 1
            If IsTrueAlgo(j) = True Then
                Me.txtResult.Text = Me.Obj.Length - j
                Me.ButPrint.IsEnabled = True
                Exit Sub
            End If
        Next
    End Sub
    Private Function GetStartIndex() As Long
        Dim Total = m * K
        Dim CurTotal As Double = 0
        For i As Long = 0 To Me.Obj.Length - 1
            CurTotal += Me.Obj(Me.Obj.Length - i - 1)
            If CurTotal > Total Then
                Return Me.Obj.Length - i - 1
                Exit For
            End If
        Next
        Return 0
    End Function
    Private Function IsTrueAlgo(_CurIndex As Long) As Boolean
        'k ≥ ai ≥ 1
        If _CurIndex + 1 > Me.Obj.Length Then
            Return True
        End If


        Dim Curm As Long = 1 'مشخصه جعبه جاری
        Dim j As Long  'مشخصه شیء جاری
        Dim CurFreeK = K 'فضای باقیمانده جعبه جاری
        For j = _CurIndex To Me.Obj.Length - 1
            'کنترل اتمام جعبه ها
            If Curm > m Then
                Return False
            End If
            'کنترل موجود بودن جای خالی
            If Me.Obj(j) <= CurFreeK Then
                'مقدار دهی فضای  باقیمانده جعبه جاری
                CurFreeK -= Me.Obj(j)
            Else
                j -= 1
                Curm += 1 'به سراغ شیئ بعدی می رویم
                CurFreeK = K 'مقدار دهی فضای باقیمانده
            End If
        Next
        Return True

    End Function
    Private Sub PrintResult()
        Dim printDlg As PrintDialog = New PrintDialog()
        Dim doc As FlowDocument = CreateFlowDocument(Me.n, Me.m, Me.txtResult.Text)
        doc.FontSize = 20
        doc.Name = "FlowDoc"
        Dim idpSource As IDocumentPaginatorSource = doc
        printDlg.PrintDocument(idpSource.DocumentPaginator, "Hello WPF Printing.")
    End Sub
#Region "Validation"
    Private Function Validation_n() As ValidationResult
        Try
            If String.IsNullOrEmpty(Me.txtn.Text) Then
                Return New ValidationResult(False, "!تعداد شیء را وارد نمایید")
            End If

            If New Regex("[^0-9]+").IsMatch(Me.txtn.Text) = True Then
                Return New ValidationResult(False, "!تعداد شیء وارد شده نامعتبر است")
            End If



            If String.IsNullOrEmpty(Me.txtn.Text) Then
                Me.n = 0 'تعداد شی 
            Else
                Me.n = CLng(Me.txtn.Text) 'تعداد شی 
            End If
            If Me.n < 1 Then
                Return New ValidationResult(False, "!تعداد شیء نمی تواند کوچکتر از صفر باشد")
            End If
            Return New ValidationResult(True, Nothing)
        Catch ex As Exception
            Return New ValidationResult(False, "!تعداد شیء وارد شده نامعتبر است")
        End Try
    End Function
    Private Function Validation_m() As ValidationResult
        Try
            If String.IsNullOrEmpty(Me.txtm.Text) Then
                Return New ValidationResult(False, "!تعداد جعبه را وارد نمایید")
            End If

            If New Regex("[^0-9]+").IsMatch(Me.txtm.Text) = True Then
                Return New ValidationResult(False, "!تعداد جعبه وارد شده نامعتبر است")
            End If


            Me.m = CLng(Me.txtm.Text) 'تعداد جعبه 



            If Me.m < 1 Then
                Return New ValidationResult(False, "!تعداد جعبه نمی تواند کوچکتر از صفر باشد")
            End If

            Return New ValidationResult(True, Nothing)
        Catch ex As Exception
            Return New ValidationResult(False, "!تعداد جعبه وارد شده نامعتبر است")
        End Try
        
    End Function
    Private Function Validation_K() As ValidationResult
        Try
            If String.IsNullOrEmpty(Me.txtk.Text) Then
                Return New ValidationResult(False, "!اندازه جعبه را وارد نمایید")
            End If

            If New Regex("[^0-9.]+").IsMatch(Me.txtk.Text) = True Then
                Return New ValidationResult(False, "!اندازه جعبه نامعتبر است")
            End If

            Me.K = CDbl(Me.txtk.Text) 'اندازه جعبه


            If Me.K < 1 Then
                Return New ValidationResult(False, "!اندازه جعبه نمی تواند کوچکتر از 1 باشد")
            End If


            Return New ValidationResult(True, Nothing)
        Catch ex As Exception
            Return New ValidationResult(False, "!اندازه جعبه نامعتبر است")
        End Try
      
    End Function
    Private Function Validation_a() As ValidationResult
        Try
            If String.IsNullOrEmpty(Me.txta.Text) Then
                Return New ValidationResult(False, "!اندازه اشیاء را وارد نمایید")
            End If

            Dim _SizeObjects = Me.txta.Text.Split(",")


            If _SizeObjects.Length <> Me.n Then
                Return New ValidationResult(False, "!تعداد اندازه اشیاء باید با تعداد شیء برابر باشد" & "تعداد شیء " & Me.n & " تعداد اندازه اشیاء " & _SizeObjects.Length)
            End If

            Me.Obj = New Double(_SizeObjects.Length - 1) {}

           

            For i As Long = 0 To _SizeObjects.Length - 1
                If String.IsNullOrEmpty(_SizeObjects(i)) Then
                    Return New ValidationResult(False, "!اندازه شیء شماره " & i + 1 & " را وارد نمایید")
                End If

                If New Regex("[^0-9.]+").IsMatch(_SizeObjects(i)) = True Then
                    Return New ValidationResult(False, "!اندازه شیء شماره " & i + 1 & " نامعتر است")
                End If
                If _SizeObjects(i) < 1 Then
                    Return New ValidationResult(False, "!اندازه شیء شماره " & i + 1 & " نمی تواند کوچکتر از 1 باشد")
                End If

                If _SizeObjects(i) > K Then
                    Return New ValidationResult(False, "!اندازه شیء شماره " & i + 1 & " نمی تواند بزرگتر از اندازه جعبه باشد")
                End If

                Me.Obj(i) = _SizeObjects(i)
            Next


            Return New ValidationResult(True, Nothing)
        Catch ex As Exception
            Return New ValidationResult(False, "!اندازه اشیاء نامعتبر است")
        End Try
       
    End Function
    Private Function Validation() As Boolean
        Dim _ValidationResult As ValidationResult

        _ValidationResult = Me.Validation_n()
        If _ValidationResult.IsValid = False Then
            Me.txtn.Focus()
            MsgBox(_ValidationResult.ErrorContent)
            Return False
        End If

        _ValidationResult = Validation_m()
        If _ValidationResult.IsValid = False Then
            Me.txtm.Focus()
            MsgBox(_ValidationResult.ErrorContent)
            Return False
        End If

        _ValidationResult = Validation_K()
        If _ValidationResult.IsValid = False Then
            Me.txtk.Focus()
            MsgBox(_ValidationResult.ErrorContent)
            Return False
        End If


        _ValidationResult = Validation_a()
        If _ValidationResult.IsValid = False Then
            Me.txta.Focus()
            MsgBox(_ValidationResult.ErrorContent)
            Return False
        End If


        Return True
    End Function
#End Region
   
    




    Private Function CreateFlowDocument(_CountOfObject As Integer, _CountOfBox As Integer, _StartResult As Integer) As FlowDocument
        Dim flowDoc = New FlowDocument()
        ' Create the Table...
        Dim table1 = New Table()
        ' ...and add it to the FlowDocument Blocks collection.
        flowDoc.Blocks.Add(table1)

        ' Set some global formatting properties for the table.
        table1.CellSpacing = 10
        table1.BorderThickness = New Thickness(2)
        table1.BorderBrush = New SolidColorBrush(Colors.Black)

        ' Create 6 columns and add them to the table's Columns collection.
        table1.Columns.Add(New TableColumn())
        table1.Columns.Add(New TableColumn())

        ' Create and add an empty TableRowGroup to hold the table's Rows.
        table1.RowGroups.Add(New TableRowGroup())

        ' Add the first (title) row.
        table1.RowGroups(0).Rows.Add(New TableRow())

        ' Alias the current working row for easy reference.
        Dim currentRow As New TableRow()
        currentRow = table1.RowGroups(0).Rows(0)
        currentRow.Background = New SolidColorBrush(Colors.YellowGreen)

        ' Add cells with content to the second row.
        currentRow.Cells.Add(New TableCell(New Paragraph(New Run("ورودی نمونه"))))
        currentRow.Cells.Add(New TableCell(New Paragraph(New Run("خروجی نمونه"))))


        table1.RowGroups(0).Rows.Add(New TableRow())
        currentRow = table1.RowGroups(0).Rows(1)
        currentRow.Background = New SolidColorBrush(Colors.Yellow)


        ' Add cells with content to the third row.
        currentRow.Cells.Add(New TableCell(New Paragraph(New Run(_CountOfObject))))
        currentRow.Cells.Add(New TableCell(New Paragraph(New Run(_StartResult))))




        table1.RowGroups(0).Rows.Add(New TableRow())
        currentRow = table1.RowGroups(0).Rows(2)
        currentRow.Background = New SolidColorBrush(Colors.Yellow)
        ' Add cells with content to the third row.
        currentRow.Cells.Add(New TableCell(New Paragraph(New Run(_CountOfBox))))


        Return flowDoc
    End Function

    Private Sub ButPrint_Click()
        If Not String.IsNullOrEmpty(Me.txtResult.Text) Then
            Me.PrintResult()
        End If

    End Sub
    Private Sub TextChanged(sender As Object, e As TextChangedEventArgs) Handles txtn.TextChanged, txtm.TextChanged, txtk.TextChanged, txta.TextChanged
        Me.ButPrint.IsEnabled = False
        Me.txtResult.Text = String.Empty
    End Sub

    Private Sub txt_LostKeyboardFocus(sender As Object, e As KeyboardFocusChangedEventArgs) Handles txtn.LostKeyboardFocus, txtm.LostKeyboardFocus, txtk.LostKeyboardFocus, txta.LostKeyboardFocus
        Dim txt = TryCast(sender, TextBox)
        Dim _ValidationResult As ValidationResult

        If txt.Equals(txtn) Then
            _ValidationResult = Me.Validation_n()
            Me.LblErorn.Content = _ValidationResult.ErrorContent
        End If
        If txt.Equals(txtm) Then
            _ValidationResult = Me.Validation_m()
            Me.LblErorm.Content = _ValidationResult.ErrorContent
        End If
        If txt.Equals(txtk) Then
            _ValidationResult = Me.Validation_K()
            Me.LblErork.Content = _ValidationResult.ErrorContent
        End If

        If txt.Equals(txta) Then
            _ValidationResult = Me.Validation_a()
            Me.LblErora.Content = _ValidationResult.ErrorContent
        End If

        
        If IsNothing(Me.LblErorn.Content) And IsNothing(Me.LblErorm.Content) And IsNothing(Me.LblErork.Content) And IsNothing(Me.LblErora.Content) Then
            If Not String.IsNullOrEmpty(Me.txtn.Text) And Not String.IsNullOrEmpty(Me.txtm.Text) And Not String.IsNullOrEmpty(Me.txtk.Text) And Not String.IsNullOrEmpty(Me.txta.Text) Then
                Me.ButSave.IsEnabled = True
            Else
                Me.ButSave.IsEnabled = False
            End If
        Else
            Me.ButSave.IsEnabled = False
        End If

    End Sub
End Class
