      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0213.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0213.CPv  $
      *
      *   Rev 2.2   Jan 03 2018 10:29:44   t13ae37
      *APA02492: 3. Statistik-Kategorie Folge Neu
      *
      *   Rev 2.1   Nov 21 2017 10:22:28   t13ae37
      *APA02492: 3. Statistik-Kategorie Folge Neu - ABU Copy
      *
      *   Rev 2.0   Feb 27 1999 11:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   13 Nov 1998 09:07:20   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   05 Aug 1998 18:36:02   T13EX22
      *APAA0032 - PEW Release 2.0
      *
      *   Rev 1.0   03 Dec 1997 15:47:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0213 FUNKTION ERSTELLEN PROTOKOLL        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    18.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!18.12.97! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      * AFPA0012!23.09.98! 01 ! ANPASSUNG VERSION 3.0     ! LÜTTGES  *
      * APA02492!23.10.17!2.1 ! ERWEITERUNG WG. FOLGE-NEU ! BUCH     *
      * APA02492!03.01.18!2.2 ! RÜCKGÄNGIG GEMACHT        ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0213                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0213.
           05  PI-DRAT0213-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=write-first  2=write-next
          05 PI-DRAT0213-AUSWAHL-KRITERIEN.
             10 PI-DRAT0213-KNR-PROT-ART     PIC S9(004) BINARY.
             10 PI-DRAT0213-PROT-ERSTELL-DAT PIC  X(010).
             10 PI-DRAT0213-KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
             10 PI-DRAT0213-PROT-PTR-ID      PIC S9(015) PACKED-DECIMAL.
             10 PI-DRAT0213-KNR-PROT-PTR-OBJ PIC S9(004) BINARY.
             10 PI-DRAT0213-KNR-PROT-PTR-AUSPR PIC S9(004) BINARY.
 
          05 PI-DRAT0213-DATEN.
             10 PI-DRAT0213-ANZAHL             PIC S9(004)  BINARY.
231017*      10 PI-DRAT0213-TABELLE OCCURS 10.
231017*      10 PI-DRAT0213-TABELLE OCCURS 16.
030118       10 PI-DRAT0213-TABELLE OCCURS 10.
                15 PI-DRAT0213-PROZ-TEXT       PIC  X(080).
                15 PI-DRAT0213-PROZ-BETRAG-JN  PIC  X(001).
                15 PI-DRAT0213-PROZ-BETRAG     PIC S9(009)V99
                                                   PACKED-DECIMAL.
      *
       01 PO-DRAT0213.
          05 PO-DRAT0213-RC                    PIC  X(003).
             88 PO-DRAT0213-RC-OK                   VALUE 'OK '.
      *
      ******** Ende Copy-Member **************************************
 

