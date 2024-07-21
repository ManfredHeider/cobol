      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  12.02.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3769.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3769.CPv  $
      *
      *   Rev 2.1   Mar 30 2020 08:16:12   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.0   Feb 12 2020 08:20:48   t13ae31
      *
      *    SATZLÄNGE 3008
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AKI01766!12.02.20!    ! neu                       !P. Hübner  *
      * DBO-8432!20.04.21!    ! neue Felder               !P. Hübner  *
      *---------------------------------------------------------------*
CCC009*   SKIP1
       01  DRKI3769.
         10  ADV-CSATZ.
           20  ADV-SA              PIC X.
           20  ADV-ORGANR1.
             30  ADV-GES         PIC XX.
             30  ADV-FD          PIC X(3).
             30  ADV-BD          PIC X(3).
             30  ADV-VBNR        PIC X(8).
           20  ADV-VNR             PIC 9(11).
           20  ADV-PNR             PIC X(15).
           20  ADV-SPARTE          PIC X.
           20  ADV-BAKKZ           PIC X.
           20  ADV-VBEG            PIC 9(8).
           20  ADV-VABL            PIC 9(8).
           20  ADV-AEND            PIC 9(8).
           20  ADV-RUHVON          PIC 9(8).
           20  ADV-RUHBIS          PIC 9(8).
           20  ADV-STORNODAT       PIC 9(8).
           20  ADV-STORNOGR        PIC XX.
           20  ADV-ZAHLW           PIC X.
           20  ADV-FAEL.
             30  ADV-FAELJ       PIC XX.
             30  ADV-FAELM       PIC XX.
           20  ADV-LAST            PIC X.
           20  ADV-AVB             PIC XX.
           20  ADV-TARIF           PIC XXX.
           20  ADV-BVTR            PIC X.
           20  ADV-VSU             PIC S9(7).
           20  ADV-KLAUS           PIC XX   OCCURS 6.
           20  ADV-ANREDE          PIC X.
           20  ADV-TITEL           PIC X(30).
           20  ADV-NAME            PIC X(30).
           20  ADV-PLZ             PIC X(7).
           20  ADV-ORT             PIC X(25).
           20  ADV-STR             PIC X(30).
           20  ADV-ZUPOST          PIC X(30).
           20  ADV-GEBD            PIC 9(8).
           20  ADV-PRAEMIE         PIC S9(7)V99.
           20  ADV-SCHADENTAB      OCCURS 3.
             30 ADV-SANZ         PIC 9(5).
             30 ADV-SDM          PIC 9(9).
           20  ADV-AKTKZ           PIC X(4).
           20  ADV-KZ.
             30  ADV-CARD        PIC X.
             30  ADV-DATBIS      PIC X(6).
           20  ADV-RING.
             30  ADV-VORVERS     PIC 9(11).
             30  ADV-WVERTR1     PIC S9(11).
             30  ADV-WVERTR2     PIC S9(11).
           20  ADV-AUSL            PIC X(4).
           20  ADV-ANGEST-KZ       PIC XX.
           20  ADV-ROL-VNR         PIC X(11).
           20  ADV-LKZ             PIC X(03).
           20  ADV-BLZ             PIC X(08).
           20  ADV-KTONR           PIC X(10).
           20  ADV-PLZ-ALT         PIC X(04).
           20  ADV-GST2                     PIC X(03).
           20  ADV-VERMITTLER2              PIC X(06).
           20  ADV-ANTEIL-PROV              PIC X(01).
           20  ADV-AENDERUNGS-KZ            PIC X(02).
           20  ADV-BETR-GESCHAEFTSSTELLE    PIC X(04).
           20  ADV-BETR-BEZIRK              PIC X(06).
           20  FILLER                       PIC X(16).
           20  ADV-KUNDENBONUS              PIC X(01).
           20  ADV-DAUERNACHLASS            PIC X(01).
           20  ADV-VSPNACHLASS              PIC X(01).
           20  ADV-OEFFDIENSTNL             PIC X(01).
           20  ADV-WKZ                      PIC X(03).
           20  ADV-SOFORTRABATT             PIC X(02).
           20  ADV-MAND-REF                 PIC X(18).
           20  ADV-MAND-STATUS              PIC X(01).
           20  ADV-BIC                      PIC X(11).
           20  ADV-IBAN                     PIC X(34).
           20  ADV-ANREDE-BZ                PIC X(01).
           20  ADV-NACHNAME-BZ              PIC X(30).
           20  ADV-VORNAME-BZ               PIC X(30).
           20  ADV-PLZ-BZ                   PIC X(07).
           20  ADV-ORT-BZ                   PIC X(30).
           20  ADV-STR-BZ                   PIC X(30).
           20  ADV-GEBD-BZ                  PIC 9(08).
 *
           20  ADV-RISZAEHL        PIC 9(3).
           20  ADV-KORRMAKLER      PIC 9(1).
           20  ADV-MITARB-RAB      PIC 9(2).
           20  ADV-MITARB-ID       PIC X(12).
           20  ADV-VORNAME         PIC X(30).
           20  ADV-PRAEMIE-NETTO   PIC S9(7)V99.
           20  ADV-ANTRAGS-ART     PIC XX.   
           20 ADV-ANNAHME-DAT     PIC 9(8). 
           20 ADV-UMST-FAEHIGKEIT PIC X.    
           20 FILLER              PIC X(85).
           
           20  ADV-VARIABEL OCCURS 10 DEPENDING ON ADV-RISZAEHL.
             30 ADV-LFDNR        PIC 9(3).
             30 ADV-RISBEG       PIC 9(8).
             30 ADV-RISAEND      PIC 9(8).
             30 ADV-WAG          PIC X(4).
             30 ADV-TARIFGR      PIC XXX.
             30 ADV-VART         PIC X(3).
             30 ADV-VTEXT        PIC X(25).
             30 ADV-STCK         PIC 9(5).
             30 ADV-RISBTR       PIC S9(7)V99.
             30 ADV-RISIKO.
                40  ADV-RISTXT1  PIC X(25).
                40  ADV-RISTXT2  PIC X(20).
             30 ADV-SELBSTBETEIL PIC S9(7).
             30 ADV-SELBSTBETEIL-X REDEFINES ADV-SELBSTBETEIL
                                 PIC  X(7).
             30 ADV-SFRSTUFE     PIC  X(1).
             30 ADV-SFRVARIANTE  PIC  X(1).
             30 ADV-RISBTR-NETTO PIC S9(7)V99.
             30 FILLER           PIC X(87).
       01  ADV-ASATZ.
          10  ADV-SA-A                PIC X.
          10  ADV-ABS                 PIC X(27).
          10  ADV-ERSTDAT.
            20  ADV-ERST-TT         PIC 9(2).
            20  ADV-ERST-MM         PIC 9(2).
            20  ADV-ERST-JJJJ       PIC 9(4).
          10  ADV-ABSDAT              PIC 9(8).
          10  ADV-EMPF                PIC X(27).
          10  ADV-INHALT              PIC X(20).
          10  ADV-WAE-KZ              PIC X(3).
          10  FILLER                  PIC X(2910).

       01  ADV-ESATZ.
          10  ADV-SA-E                PIC X.
          10  ADV-SATZANZ             PIC 9(7).
          10  ADV-SATZSUM-E OCCURS 5.
            20 ADV-UEBERLAUF        PIC X(1).
            20 ADV-SATZSUM          PIC 9(15)V99.
          10  FILLER                  PIC X(2906).


