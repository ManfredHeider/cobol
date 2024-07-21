      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0284.cpy  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\drat0284.cpv  $
      *
      *   Rev 2.3   May 04 1999 13:10:04   T13AE18
      *APA00012 Erweiterungen
      *
      *   Rev 2.2   Apr 21 1999 09:18:48   T13AE18
      *APA00012 Neue Kinstante eingefügt
      *
      *   Rev 2.1   Mar 25 1999 12:58:12   T13AE18
      *APA00012
      *Neue Ausprägungen definiert
      *
      *   Rev 2.0   Feb 27 1999 11:57:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   13 Jul 1998 09:29:04   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0284  KONSTANTEN FÜR                     *
      *                           DAISY-DIALOG-BEARBEITUNGFUNKTIONEN *
      *                                                    KTY = 171 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    15.03.1999                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00012!15.03.99! 01  ! NEUERSTELLUNG             !BOHLINGER*
      * APA00012!13.04.99! 02  ! NEU 'FREIGABE-KORREKTUR'  !BOHLINGER*
      * APA00012!29.04.99! 03  ! NEU 'DIALOG-IN-ARBEIT'    !BOHLINGER*
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-DIA-KORR-A-PEST         PIC S9(004) BINARY VALUE 1.
           05 K-KNR-DIA-KORR-EE-ENT         PIC S9(004) BINARY VALUE 2.
           05 K-KNR-DIA-FREIGABE-EVS-AUSG   PIC S9(004) BINARY VALUE 3.
           05 K-KNR-DIA-U-PEST-PT-AEND      PIC S9(004) BINARY VALUE 4.
           05 K-KNR-DIA-U-PEST-PT-COPY      PIC S9(004) BINARY VALUE 5.
           05 K-KNR-DIA-FREIGABE-EVS-VSTR   PIC S9(004) BINARY VALUE 6.
           05 K-KNR-DIA-KORR-U-PEST-VERTR   PIC S9(004) BINARY VALUE 7.
           05 K-KNR-DIA-KORR-VM-U-PEST-AP   PIC S9(004) BINARY VALUE 8.
           05 K-KNR-DIA-UMB-PEST-DGV        PIC S9(004) BINARY VALUE 9.
           05 K-KNR-DIA-UMB-DGV             PIC S9(004) BINARY VALUE 10.
           05 K-KNR-DIA-W-UMB               PIC S9(004) BINARY VALUE 11.
           05 K-KNR-DIA-KORR-FAELLIG-B-OPAL PIC S9(004) BINARY VALUE 12.
           05 K-KNR-DIA-FREIGABE-UMBUCHUNG  PIC S9(004) BINARY VALUE 13.
130499     05 K-KNR-DIA-FREIGABE-KORREKTUR  PIC S9(004) BINARY VALUE 14.
290499     05 K-KNR-DIA-IN-ARBEIT           PIC S9(004) BINARY VALUE 15.
