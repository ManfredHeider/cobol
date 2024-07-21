      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1472.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1472.CPv  $
      *
      *   Rev 2.5   Aug 30 2010 13:32:50   t13ae37
      *APA00994: CoBa - DreBa DTA Trennung
      *
      *   Rev 2.4   Aug 30 2010 10:42:00   t13ae37
      *APA00994: CoBa - DreBa DTA Trennung
      *
      *   Rev 2.3   Aug 27 2010 15:54:00   t13ae37
      *APA00994: CoBa - DreBa DTA Trennung
      *
      *   Rev 2.2   Aug 27 2010 15:36:40   t13ae37
      *APA00994: CoBa - DreBa DTA Trennung
      *
      *   Rev 2.1   20 Mar 2001 15:24:58   T13EX21
      *APA00131 - Neuerstellung
      *
      *   Rev 2.0   09 Jan 2001 10:22:06   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1472  PEG-DATENSATZ DER COMMERZBANK (68)  *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    21.02.01                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00131!21.02.01!2.1 ! NEUERSTELLUNG             ! STORCH   *
      * APA00994!25.01.10!2.2 ! ANPASSUNG NEUER COBA-SATZ ! BUCH     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    COMMERZBANK-DTA'S.                                        *
      *    FELDER, DIE MIT "NR-" BEGINNEN, SIND FÜR DAISY NICHT      *
      *    RELEVANT.                                                 *
      ****************************************************************
      *
             30 COMMERZBANK-DATENSATZ.
                35  NR-KOOPNR               PIC S9(11) COMP-3.
                35  NR-MAD-NR.
                    40  FILLER              PIC X(03).
                    40  AVERM               PIC X(07).
                    40  NR-VBNR-PRZ         PIC X(01).
                    40  FILLER              PIC X(04).
                35  NR-INFOFELD             PIC X(50).
                35  PGV-PG.
                    40  PROV-GRUND          PIC X(01).
                    40  VORGANG-KZ          PIC X(01).
                35  FILLER                  PIC X(26).
                35  VERTRAG-BEGINN          PIC S9(09) COMP-3.
                35  QUALITAETS-KZ           PIC X(01).
                35  PROD-BEZ.
                    40  PROD-BEZ-1          PIC X(08).
                    40  PROD-BEZ-2          PIC X(08).
                35  NR-VERTRAG.
                    40  VERTRAGSSUMME       PIC S9(15)V9(02) COMP-3.
                    40  PPFL-SUMME          PIC S9(15)V9(02) COMP-3.
                    40  VERTRAG-WAEHRUNG    PIC X(03).
                    40  NR-VT-LAUFZEIT-JJ   PIC S9(03) COMP-3.
                    40  NR-VT-LAUFZEIT-MM   PIC S9(03) COMP-3.
                    40  NR-VT-LAUFZEIT-TT   PIC S9(03) COMP-3.
                    40  NR-VT-BEGINN        PIC S9(09) COMP-3.
                    40  NR-VT-ENDE          PIC S9(09) COMP-3.
                35  NR-MARGE                PIC S9(15)V9(02) COMP-3.
                35  FILLER                  PIC X(32).
                35  NR-PROVISION.
                    40  PROV-TYP            PIC X(01).
                    40  NR-PROV-BETRAG      PIC S9(15)V9(02) COMP-3.
                    40  PROV-SATZ           PIC S9(03)V9(05) COMP-3.
                    40  QUALIBETR           PIC S9(15)V9(02) COMP-3.
                    40  PROVGES-BETRAG      PIC S9(15)V9(02) COMP-3.
                    40  ABR-DAT-PG.
                        45 FAEL-JH          PIC X(02).
                        45 FAEL-JJ          PIC X(02).
                        45 FAEL-MM          PIC X(02).
                        45 FAEL-TT          PIC X(02).
                    40  FILLER              PIC X(08).
250110*             40  NR-GUTKONTOCB       PIC S9(13) COMP-3.
250110              40  NR-GUTKONTOCB       PIC S9(10) COMP-3.
250110              40  BLZ                 PIC S9(08) COMP-3.
                    40  NR-PROV-WAEHR       PIC X(03).
                35  AENDERUNGSGRUND         PIC X(02).
250110*         35  FILLER                  PIC X(39).
250110          35  FILLER                  PIC X(35).
                35  NR-KUNDENDATEN.
                    40  VNR                 PIC S9(11) COMP-3.
                    40  NR-KD-ANREDE        PIC S9(03) COMP-3.
                    40  KUNDE-NNAME         PIC X(29).
                    40  NR-KD-NAME2         PIC X(29).
                    40  NR-KD-VORNAME       PIC X(29).
                    40  NR-KD-NACHNAME      PIC X(29).
                    40  NR-KD-STRASSE       PIC X(29).
                    40  NR-KD-ORT           PIC X(29).
                    40  NR-KD-PLZ           PIC X(10).
                    40  NR-KD-WOHNORT       PIC X(30).
                    40  NR-KD-GEBDAT        PIC S9(09) COMP-3.
                    40  NR-KD-LAND-KZ       PIC S9(03) COMP-3.
                    40  VNR-BAUFI           PIC X(16).
                    40  NR-KD-VTNR-PARTNER  PIC X(20).
250110          35 FILLER                   PIC X(35).
250110*         35 FILLER                   PIC X(31).

      *
      ******** ENDE COPY-MEMBER **************************************
