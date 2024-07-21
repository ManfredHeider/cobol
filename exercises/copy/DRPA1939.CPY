      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1939.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1939.CPv  $
      *
      *   Rev 2.2   Mar 30 2020 15:31:14   t13ex44
      *FPA01770: Korrektur DRPA1939 & Compile UWPA0019
      *
      *   Rev 2.1   Oct 08 2019 12:46:54   t13ex44
      *APA02886: Einheitenprüfung - Mehrfache Einheiten auf einen V
      *B unzulässig
      *
      *   Rev 2.0   Oct 02 2019 10:56:14   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1939  EINHEITEN JER EMP-TYP              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    02.10.2019                                      *
      ****************************************************************
      * DIESE TABELLE MUSS IN IHRER REIHENFOLGE MIT DERJENIGEN IM
      * DRAT0117 UEBEREIN STIMMEN.
      * ES WIRD FUER JEDEN EMPFAENGERTYP DEFINIERT, OB ER ALS SOLCHER
      * EINHEITEN BEKOMMT ODER NICHT.
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02886!18.02.19!2.0 ! NEU:                      ! SCHEIDEL *
      * FPA01770!30.03.20!2.2 ! KORREKTUR                 ! SCHEIDEL*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        01 DRPA1939.
           05 K-EINH-EMP-TYP.
      * DER EMP-TYP "UNBEKANNT" HAT IM DRAT0117 DEN INDEX "0" UND
      * DARF DAHER HIER NICHT BERUECKSICHTIGT WERDEN.
      *       10 K-EINH-UNBEKANNT      PIC S9(004) BINARY  VALUE  0.
              10 K-EINH-VTF            PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-VWF            PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-VM             PIC S9(004) BINARY  VALUE  0.
              10 K-EINH-KD-BETR        PIC S9(004) BINARY  VALUE  0.
              10 K-EINH-BETREUER       PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-NG-STRUK       PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-AVERM          PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-AVERM-O-P      PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-AVERM-P2       PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-AVERM-P3       PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-AVERM-P4       PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-AVERM-P5       PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-AVERM-P6       PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-AVERM-P7       PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-AVERM-ALT      PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-VBUE           PIC S9(004) BINARY  VALUE  0.
              10 K-EINH-VBUE-ALT       PIC S9(004) BINARY  VALUE  0.
              10 K-EINH-KDUE           PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-KUEF           PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-QUER           PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-OAE            PIC S9(004) BINARY  VALUE  0.
              10 K-EINH-N1EM           PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-N1EM-ALT       PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-N1EO           PIC S9(004) BINARY  VALUE  0.
              10 K-EINH-N1EO-ALT       PIC S9(004) BINARY  VALUE  0.
              10 K-EINH-N2EO           PIC S9(004) BINARY  VALUE  0.
              10 K-EINH-N2EO-ALT       PIC S9(004) BINARY  VALUE  0.
              10 K-EINH-N1GM           PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-N1GO           PIC S9(004) BINARY  VALUE  0.
              10 K-EINH-N2GM           PIC S9(004) BINARY  VALUE  1.
              10 K-EINH-N2GO           PIC S9(004) BINARY  VALUE  0.
 
           05 K-EINH-EMP-TYP-TABELLE REDEFINES K-EINH-EMP-TYP.
              10 K-EINH-EMP-TYP-ZEILE  OCCURS 31
                                       INDEXED BY I-EINH-EMP-TYP.
                 15 K-EINH-EMP-TYP-WERT
                                       PIC S9(004) BINARY.
                    88 K-EINH-EMP-TYP-NEIN         VALUE 0.
                    88 K-EINH-EMP-TYP-JA           VALUE 1.


