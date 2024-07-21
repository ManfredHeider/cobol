      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0004.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0004.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Nov 1998 16:53:22   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Nov 04 1995 08:34:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00001000
      *    COPY-MEMBER: DRAT0004                                      * 00002000
      *    --------------------------------------------------------   * 00003000
      *    STANDARD-ERROR-BEREICH (INCL. TRACE)                       * 00004000
      *    --------------------------------------------------------   * 00005000
      *    ERSTELLT: SCHÖLLHAMMER (INTEGRATA AG)                      * 00006000
      *    DATUM:    03.11.1992                                       * 00007000
      ***************************************************************** 00008000
      *    VERSION   2    10.03.95 15:57:20
      ****************************************************************  00009100
      * AENDERUNGEN:                                                 *  00009200
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00009300
      * -------------------------------------------------------------*  00009400
      *         !21.04.93! 01 ! NEU                       ! SCHÖLLH. *  00009500
      * AFVB0144!19.08.94!    ! REDEF. AUF ERR-VAR NEU    ! SCHÖLLH. *  00009701
      *         !06.09.94! 02 ! PROD.UEBERGABE            ! SCHÖLLH. *  00009803
      * FEVB0218!03.03.95!    ! 3. REDEF. AUF ERR-VAR-RAHM! SCHÖLLH. *  00009900
      *         !10.03.95! 03 ! PROD.UEBERGABE            ! SCHÖLLH. *  00070500
      * APA00012!12.08.98! 04 ! ERR-BEREICH ERGäNZT UM    ! AUERBACH *  00070500
      *         !        !    ! CICS-NAME UND USER-ID     !          *  00010000
      *         !        !    !                           !          *  00010000
      *                                                              *  00010100
      * ENDE-LOGBUCH                                                 *  00010200
      ****************************************************************  00010300
      *                                                                 00100000
      *    DER ERROR-BEREICH DIENT ALS SCHNITTSTELLE                    00110000
      *    - BEI UNTERPROGRAMM-AUFRUFEN                                 00120000
      *    - ZUM AUFBEREITUNGSPROGRAMM BEI SCHWEREN FEHLERN (UPAT0001)  00130000
      *                                                                 00140000
      *    ERR-RC: PFLICHT-RUECKGABE BEI UNTERPROGRAMM-AUFRUFEN         00150000
      *    ------                                                       00160000
      *                                                                 00170000
      *       FALLS DER ERR-RC NICHT = ERR-RC-OK IST, WERDEN            00180000
      *       VOM UNTERPROGRAMM - ABHAENGIG VON ERR-FEHLER-TYP          00190000
      *       ZUSAETZLICH FOLGENDE WERTE ZURÜCKGEGEBEN:                 00200000
      *       ---------------------+-----+-----+-----+-----+-----+-----+00210000
      *       ERR-FEHLER-TYP       ! SQL !CICS !PROG !DATEI!MVS- !ROLL !00220000
      *                            !     !     !     !     !SYST.!BACK !00230000
      *       ---------------------+-----+-----+-----+-----+-----+-----+00240000
      *       ERR-SQLCODE          !  X  !     !     !     !     !  X  !00250000
      *       ERR-SQLERRMC         !  X  !     !     !     !     !  X  !00260000
      *       ERR-CICS-RC          !     !  X  !     !     !     !     !00270000
      *       ERR-FILE-STATUS      !     !     !     !  X  !     !     !00280000
      *       ERR-UPRONAME         !  X  !  X  !  X  !  X  !  X  !  X  !00290000
      *       ERR-MELDUNG-KEY (OPT)!     ! (X) ! (X) ! (X) ! (X) !     !00300000
      *       ERR-VAR (OPTIONAL)   ! (X) ! (X) ! (X) ! (X) ! (X) ! (X) !00310000
      *       ERR-ORT-LFD (OPT.)   ! (X) ! (X) ! (X) ! (X) ! (X) ! (X) !00320000
      *       ---------------------+-----+-----+-----+-----+-----+-----+00330000
      *       HINWEIS: DER 'ERR-FTYP-DB2S' WIRD NIE VOM UPRO ZURÜCK-    00340000
      *                GEGEBEN                                          00341000
      * -------------------------------------------------------------   00350000
       01  ERR-ERROR-BEREICH.                                           00360000
           05  ERR-RC                    PIC  9(001).                   00370000
               88  ERR-RC-OK                          VALUE ZERO.       00380000
               88  ERR-RC-SCHWERER-FEHLER             VALUE 4.          00390000
                                                                        00391000
      * -------------------------------------------------------------   00393000
      * ZU FTYP-SQL UND FTYP-ROLLBACK:                                  00394000
      *   IN EINIGEN UNTERPROGRAMMEN (Z.B. DB2-SCHREIBMODULN) WIRD      00395000
      *   ZWISCHEN DIESEN BEIDEN TYPEN UNTERSCHIEDEN:                   00396000
      *   - SQL: SQL-FEHLER, ABER KEINE INKONSISTENZ INNERHALB DIESES   00397000
      *          UPRO-AUFRUFS --> HAUPTPROG ENTSCHEIDET UEBER ROLLBACK  00398000
      *   - ROLLBACK: SQL-FEHLER MIT INKONSISTENZ INNERHALB DIESES      00399000
      *          UPRO-AUFRUFS --> ROLLBACK ZWINGEND                     00400000
      * ZU FTYP-DB2S (DB2-SCHREIBMODUL-FEHLER):                         00400100
      *   DIESER TYP WIRD VOM PGM GESETZT, ABER VOR DER                 00400200
      *   FEHLERAUFBEREITUNG VOM FEHLERHANDLING UMGESETZT IN            00400300
      *   ERR-FTYP-SQL, ERR-FTYP-ROLLBACK ODER ERR-FTYP-PROG            00400400
      * -------------------------------------------------------------   00401000
           05  ERR-FEHLER-TYP            PIC  9(001).                   00410000
               88  ERR-FTYP-SQL                       VALUE 1.          00420000
               88  ERR-FTYP-CICS                      VALUE 2.          00430000
               88  ERR-FTYP-PROG                      VALUE 3.          00440000
               88  ERR-FTYP-DATEI                     VALUE 4.          00450000
               88  ERR-FTYP-MVS-SYSTEM                VALUE 5.          00460000
               88  ERR-FTYP-ROLLBACK                  VALUE 6.          00470000
                                                                        00472000
           05  ERR-PROGNAME              PIC  X(008).                   00480000
           05  ERR-UPRONAME              PIC  X(008).                   00490000
           05  ERR-ORT.                                                 00500000
               10  ERR-ORT-SEC           PIC  X(005).                   00510000
               10  ERR-ORT-LFD           PIC  9(003).                   00520000
           05  ERR-MELD-KEY.                                            00530000
               10  ERR-MELD-PGM          PIC  X(008).                   00540000
               10  ERR-MELD-TYP          PIC  X(001).                   00550000
               10  ERR-MELD-NR           PIC  9(004) COMP.              00560000
           05  ERR-SQLCODE               PIC S9(009) COMP.              00570000
           05  ERR-SQLERRMC              PIC  X(070).                   00580000
           05  ERR-CICS-RC               PIC S9(004) COMP.              00590000
                                                                        00600000
           05  ERR-TRACE.                                               00610000
               10  ERR-TRACE-VORN        PIC  X(225).                   00620000
               10  ERR-TRACE-NEU         PIC  X(005).                   00630000
           05  FILLER REDEFINES ERR-TRACE.                              00640000
               10  FILLER                PIC  X(005).                   00650000
               10  ERR-TRACE-HINTEN      PIC  X(225).                   00660000
                                                                        00670000
           05  ERR-VAR-RAHMEN            PIC  X(320).                   00680000
           05  FILLER REDEFINES ERR-VAR-RAHMEN.                         00690000
               10  ERR-PARAMETER-STEUER  PIC  X(046).                   00700000
               10  ERR-PARAMETER-RETURN  PIC  X(082).                   00710000
               10  FILLER                PIC  X(192).                   00720000
           05  FILLER REDEFINES ERR-VAR-RAHMEN.                         00730000
               10  ERR-DWAT0007-INPUT    PIC  X(160).                   00740000
               10  ERR-DWAT0007-OUTPUT   PIC  X(160).                   00750000
030395     05  FILLER REDEFINES ERR-VAR-RAHMEN.                         00690000
030395         10  ERR-DIALOG            PIC  X(004).                   00700000
030395         10  ERR-DIALOG-SCHRITT    PIC  X(004).                   00700000
030395         10  ERR-BILDER-TAB        PIC  X(021).                   00710000
030395         10  ERR-CICS-NAME         PIC  X(004).                   00710000
030395         10  ERR-USERID            PIC  X(008).                   00710000
120898         10  FILLER                PIC  X(279).                   00720000
120898*030395  10  FILLER                PIC  X(291).                   00720000
                                                                        00751000
           05  ERR-VAR                   PIC  X(320).                   00760000
190894     05  FILLER REDEFINES ERR-VAR.                                00761000
190894         10  ERR-VAR-ZEILE01       PIC  X(080).                   00762000
190894         10  ERR-VAR-ZEILE02       PIC  X(080).                   00763000
190894         10  ERR-VAR-ZEILE03       PIC  X(080).                   00764000
190894         10  ERR-VAR-ZEILE04       PIC  X(080).                   00765000
                                                                        00770000
           05  ERR-FILE-STATUS           PIC  X(002).                   00780000
           05  ERR-AUFBEREITUNG          PIC  9(001).                   00790000
               88  ERR-AUFBER-NORMAL                  VALUE 0.          00800000
               88  ERR-AUFBER-DB2S                    VALUE 1.          00810000
               88  ERR-AUFBER-TSQ                     VALUE 2.          00820000
      *                                                                 00830000
      ******** ENDE COPY-MEMBER ERR-ERROR-BEREICH (DRAT0004) ********** 00840000
                                                                        00850000
