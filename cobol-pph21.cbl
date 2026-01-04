       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL-HITUNG-PPH21.
       AUTHOR GESANG PAUDRA JAYA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *variabel yang digunakan
       01  INPUT-USER    PIC 9(12).
       01  PKP           PIC 9(12)V9(3).
       01  PKP-TEMP      PIC 9(12)V9(3).
       01  PTKP          PIC 9(12)V9(3).
       01  NILAI-PAJAK   PIC 9(12)V9(3).
       01  PAJAK-DISPLAY PIC ZZZ,ZZZ,ZZ9.
       01  PILIHAN       PIC X.
       01  ENTER-KEY     PIC X.

       PROCEDURE DIVISION.
       FUNGSI-UTAMA.
       
           PERFORM UNTIL ENTER-KEY = "N" OR "n"
                PERFORM HAPUS-VARIABEL
                PERFORM INPUT-AWAL
                PERFORM PTKP-MENU
                PERFORM PROSES-HITUNG
                PERFORM TAMPILKAN-HASIL
                PERFORM TANYA-RESTART
           END-PERFORM.
           STOP RUN.

      *menghapus nilai di variabel
      *berguna untuk restart program
       HAPUS-VARIABEL.
           MOVE ZERO TO INPUT-USER PTKP PKP NILAI-PAJAK.
           MOVE ZERO TO PAJAK-DISPLAY PKP-TEMP.
           MOVE SPACES TO PILIHAN ENTER-KEY.

       INPUT-AWAL.
           DISPLAY "============================================".
           DISPLAY "Selamat Datang di Program Hitung PPh21 Tahunan".
           DISPLAY "Update Nilai terakhir Desember 2025".
           DISPLAY " ".
           DISPLAY "Masukkan gaji netto tahunan anda = ".
           ACCEPT INPUT-USER.

      *menampilkan menu PTKP
       PTKP-MENU.
           DISPLAY "Pilih Status anda untuk PTKP : ".
           DISPLAY "1. Tidak Menikah tanggungan 0".
           DISPLAY "2. Tidak Menikah tanggungan 1".
           DISPLAY "3. Menikah tanpa tanggungan".
           DISPLAY "4. Tidak Menikah tanggungan 2".
           DISPLAY "5. Tidak Menikah tanggungan 3".
           DISPLAY "6. Menikah tanggungan 1".
           DISPLAY "7. Menikah tanggungan 2".
           DISPLAY "8. Menikah tanggungan 3".
           DISPLAY "----------------------------".
           DISPLAY "Pilih dengan ketik angka (1/2/3/...): ".
           ACCEPT PILIHAN.

      *menjalankan fungsi perhitungan utama
       PROSES-HITUNG.
           PERFORM AMBIL-PTKP.
           PERFORM HITUNG-PKP.
           PERFORM HITUNG-PAJAK.

      *menampilkan hasil perhitungan pajak
       TAMPILKAN-HASIL.
           MOVE NILAI-PAJAK TO PAJAK-DISPLAY.
           DISPLAY "---------------------------------".
           DISPLAY "Estimasi Pajak tahunan anda : Rp " PAJAK-DISPLAY.
           DISPLAY " ".

       TANYA-RESTART.
           DISPLAY "Tekan ENTER untuk mulai ulang".
           DISPLAY "Atau 'N' untuk keluar program".
           ACCEPT ENTER-KEY.
      * ENTER = lanjut loop, N = keluar

      *fungsi perhitungan PKP dari menu yang user pilih
       AMBIL-PTKP.
            EVALUATE PILIHAN
                WHEN "1"
                     MOVE 54000000 TO PTKP

                WHEN "2"
                     MOVE 58500000 TO PTKP

                WHEN "3"
                     MOVE 58500000 TO PTKP

                WHEN "4"
                     MOVE 63000000 TO PTKP
                    
                WHEN "5"
                     MOVE 67500000 TO PTKP

                WHEN "6"
                     MOVE 63000000 TO PTKP

                WHEN "7"
                     MOVE 67500000 TO PTKP

                WHEN "8"
                     MOVE 72000000 TO PTKP
               
            END-EVALUATE.

      *fungsi menghitung PKP (input - ptkp)
       HITUNG-PKP.
            EVALUATE TRUE
      *jika input gaji kurang dari ptkp maka set pkp ke 0
                WHEN INPUT-USER < PTKP
                     MOVE ZERO TO PKP

      *hitung pkp dari gaji dan bulatkan
                WHEN INPUT-USER >= PTKP
                COMPUTE PKP ROUNDED = ((INPUT-USER - PTKP)/1000)*1000

            END-EVALUATE.

      *fungsi menghitung pajak berdasarkan nilai PKP  
       HITUNG-PAJAK.
            EVALUATE TRUE

      *Jika pkp 0 maka NILAI-PAJAK set ke 0
                WHEN PKP = ZERO
                     MOVE ZERO TO NILAI-PAJAK

      *Jika PKP Sampai dengan 60 Juta maka nilai pajak 5%
                WHEN PKP > ZERO AND PKP <= 60000000
                     COMPUTE NILAI-PAJAK ROUNDED = PKP * 0.05
               
                WHEN PKP > 60000000 AND PKP <= 250000000
                     COMPUTE NILAI-PAJAK ROUNDED = PKP * 0.15

                WHEN PKP > 250000000 AND PKP <= 500000000
                     COMPUTE NILAI-PAJAK ROUNDED = PKP * 0.25

                WHEN PKP > 500000 AND PKP <= 5000000000
                     COMPUTE NILAI-PAJAK ROUNDED = PKP * 0.30

                WHEN PKP > 5000000000
                     COMPUTE NILAI-PAJAK ROUNDED = PKP * 0.35
          
            END-EVALUATE.

