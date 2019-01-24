
Public Class Form1
    Dim HL As String
    Dim P As String
    Sub check(ByVal m() As Integer)

        Dim s1(3), s2(3), r1(3), r2(3), n1, n2 As Integer
        For i = 0 To 3
            n1 += Val(m(i))
            n2 += Val(m(i + 4))
        Next

        TextBox2.Text &= "第一次" & vbCrLf & "左 1 2 3 4" & vbTab & "右 5 6 7 8" & vbCrLf
        If n1 = n2 Then
            TextBox2.Text &= "左邊重量等於右邊重量" & vbCrLf & vbCrLf
            C1(Val(m(8)), Val(m(9)), Val(m(10)), Val(m(11)), m(0)) : Exit Sub
        ElseIf n1 > n2 Then
            TextBox2.Text &= "左邊重量大於右邊重量" & vbCrLf & vbCrLf
            For i = 0 To 3
                s1(i) = m(i) : s2(i) = m(i + 4)
                r1(i) = i + 1 : r2(i) = i + 5
            Next
        Else
            TextBox2.Text &= "左邊重量小於右邊重量" & vbCrLf & vbCrLf
            For i = 0 To 3
                s1(i) = m(i + 4) : s2(i) = m(i)
                r1(i) = i + 5 : r2(i) = i + 1
            Next
        End If

        Dim t1(2), t2(2), t3(1), ts1, ts2 As Integer
        t1(0) = s1(0) : t1(1) = s1(1) : t1(2) = s2(0)
        t2(0) = s1(2) : t2(1) = s1(3) : t2(2) = s2(1)
        t3(0) = s2(2) : t3(1) = s2(3)
        For i = 0 To 2
            ts1 += t1(i) : ts2 += t2(i)
        Next

        TextBox2.Text &= "第二次" & vbCrLf & "左 " & r1(0) & " " & r1(1) & " " & _
            r2(0) & vbTab & "右 " & r1(2) & " " & r1(3) & " " & r2(1) & vbCrLf
        If ts1 = ts2 Then
            TextBox2.Text &= "左邊重量等於右邊重量" & vbCrLf & vbCrLf
            TextBox2.Text &= "第三次" & vbCrLf & "左 " & r2(2) & vbTab & "右 " & r2(3) & vbCrLf
            If t3(0) > t3(1) Then
                TextBox2.Text &= "左邊重量大於右邊重量" & vbCrLf & vbCrLf
                TextBox2.Text &= "結果：偽幣較輕，第" & r2(3) & "個"
            Else
                TextBox2.Text &= "左邊重量小於右邊重量" & vbCrLf & vbCrLf
                TextBox2.Text &= "結果：偽幣較輕，第" & r2(2) & "個"
            End If
        ElseIf ts1 > ts2 Then
            TextBox2.Text &= "左邊重量大於右邊重量" & vbCrLf & vbCrLf
            TextBox2.Text &= "第三次" & vbCrLf & "左 " & r1(0) & vbTab & "右 " & r1(1) & vbCrLf
            C2(t1(0), t1(1), {r1(0), r1(1), r2(1)}) : Exit Sub
        Else
            TextBox2.Text &= "左邊重量小於右邊重量" & vbCrLf & vbCrLf
            TextBox2.Text &= "第三次" & vbCrLf & "左 " & r1(2) & vbTab & "右 " & r1(3) & vbCrLf
            C2(t2(0), t2(1), {r1(2), r1(3), r2(0)}) : Exit Sub
        End If

    End Sub

    Sub C1(ByVal n1, ByVal n2, ByVal n3, ByVal n4, ByVal rw)
        TextBox2.Text &= "第二次" & vbCrLf & "左 1 2 3" & vbTab & "右 9 10 11" & vbCrLf
        If n1 + n2 + n3 = rw * 3 Then
            TextBox2.Text &= "左邊重量等於右邊重量" & vbCrLf & vbCrLf
            TextBox2.Text &= "第三次" & vbCrLf & "左 1" & vbTab & "右 12" & vbCrLf
            If rw > n4 Then
                TextBox2.Text &= "左邊重量大於右邊重量" & vbCrLf & vbCrLf
                TextBox2.Text &= "結果：偽幣較輕，第12個"
            Else
                TextBox2.Text &= "右邊重量大於左邊重量" & vbCrLf & vbCrLf
                TextBox2.Text &= "結果：偽幣較重，第12個"
            End If
            Exit Sub
        ElseIf n1 + n2 + n3 < rw * 3 Then
            TextBox2.Text &= "左邊重量大於右邊重量" & vbCrLf & vbCrLf
            TextBox2.Text &= "第三次" & vbCrLf & "左 9" & vbTab & "右 10" & vbCrLf
            HL = "輕"
        Else
            TextBox2.Text &= "左邊重量小於右邊重量" & vbCrLf & vbCrLf
            TextBox2.Text &= "第三次" & vbCrLf & "左 9" & vbTab & "右 10" & vbCrLf
            HL = "重"
        End If
        If n1 = n2 Then
            TextBox2.Text &= "左邊重量等於右邊重量" & vbCrLf & vbCrLf
            TextBox2.Text &= "結果：偽幣較" & HL & "，第11個"
        ElseIf n1 > n2 Then
            TextBox2.Text &= "左邊重量大於右邊重量" & vbCrLf & vbCrLf
            P = IIf(HL = "重", 9, 10)
            TextBox2.Text &= "結果：偽幣較" & HL & "，第" & P & "個"
        Else
            TextBox2.Text &= "左邊重量小於右邊重量" & vbCrLf & vbCrLf
            P = IIf(HL = "輕", 9, 10)
            TextBox2.Text &= "結果：偽幣較" & HL & "，第" & P & "個"
        End If

    End Sub

    Sub C2(ByVal n1, ByVal n2, ByVal r())
        If n1 = n2 Then
            TextBox2.Text &= "左邊重量等於右邊重量" & vbCrLf & vbCrLf
            TextBox2.Text &= "結果：偽幣較輕" & "，第" & r(2) & "個"
        ElseIf n1 > n2 Then
            TextBox2.Text &= "左邊重量大於右邊重量" & vbCrLf & vbCrLf
            TextBox2.Text &= "結果：偽幣較重" & "，第" & r(0) & "個"
        Else
            TextBox2.Text &= "左邊重量小於右邊重量" & vbCrLf & vbCrLf
            TextBox2.Text &= "結果：偽幣較重" & "，第" & r(1) & "個"
        End If
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        TextBox2.Clear()
        Dim n As Integer = TextBox1.Text
        If n < 1 Or n > 12 Then TextBox2.Text = "輸入範圍1~12" : Exit Sub
        Dim w As Integer = IIf(RadioButton1.Checked, 0, 1)
        Dim st(11) As Integer
        For i = 0 To 11
            If i = n - 1 Then st(i) = w Else st(i) = IIf(w = 0, 1, 0)
        Next
        check(st)

    End Sub

   
End Class
