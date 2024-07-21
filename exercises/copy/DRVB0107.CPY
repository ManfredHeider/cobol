      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0107.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0107.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Nov 18 1996 13:33:56   T13AE28
      *AFVB0321 Erweiterung der Schnittstelle auf VSWZ-Daten
      *         D.Ehrmann
      *
      *   Rev 1.1   Oct 30 1996 11:48:38   T13AE28
      *AFVB0316 Anpassungen/Korrekturen im Rahmen des VSW96
      *         D.Ehrmann
      *
      *   Rev 1.0   04 Oct 1996 12:21:02   P13SP17
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0107, SCHNITTSTELLE UPVB0092             *
      *                 ERMITTLUNG VSW-RELEVANTER ZV-DATEN           *
      *    --------------------------------------------------------  *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    27.08.1996                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FEVB0323!27,08.96!    ! NEU                       ! D.EHRMANN*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   PI-UPVB0092-FKT
      *      PI-UOVB0092-FKT-ZV-DATEN
      *         ERMITTLUNG DER ZV-DATEN ZUR üBERGEBENEN VB-NR
      *
      *   PI-UPVB0092-VB-NR
      *         VB, FüR DEN DIE VERTRAGSDATEN ERMITTELT WERDEN SOLLEN
      *
      *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-UPVB0092.
           05  PI-UPVB0092-FKT                  PIC  X(02).
               88 PI-UPVB0092-FKT-ZV-DATEN          VALUE 'ZV'.
               88 PI-UPVB0092-FKT-VSWZ-DATEN        VALUE 'VS'.

      *    * üBERGEBENE DATEN
           05  PI-UPVB0092-VB-NR                PIC  9(07).
      *
      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-UPVB0092.

      *    * ERGEBNIS:
           05  PO-UPVB0092-RC                   PIC  X(03).
               88  PO-UPVB0092-RC-OK                VALUE 'OK '.
               88  PO-UPVB0092-RC-MA-FEHLER         VALUE '090'.
               88  PO-UPVB0092-RC-KEIN-ZVZ-SATZ     VALUE '091'.
               88  PO-UPVB0092-RC-ZVZ-FEHLER        VALUE '092'.
               88  PO-UPVB0092-RC-KEIN-VSWZ-SATZ    VALUE '093'.
               88  PO-UPVB0092-RC-VSWZ-FEHLER       VALUE '094'.
               88  PO-UPVB0092-RC-NOK               VALUE 'NOK'.

      *    * DATEN ZURÜCK

      ***** ZV-DATEN
           05  PO-UPVB0092-ZV-NEU-KAND          PIC X(01).
               88 PO-UPVB0092-KEIN-KAND             VALUE 'N'.
           05  PO-UPVB0092-ZV-VERL              PIC X(01).
           05  PO-UPVB0092-ZV-NR                PIC X(01).
           05  PO-UPVB0092-ZV-DAT-VON           PIC X(10).
           05  PO-UPVB0092-ZV-DAT-BIS           PIC X(10).
           05  PO-UPVB0092-ZV-KUE-DAT           PIC X(10).
           05  PO-UPVB0092-ZV-ZVII-ABL          PIC X(01).

      ***** VSWZ-DATEN
      *    05  PO-UPVB0092-VSWZ-G-VERS-SUM      PIC S9(09)V99 COMP-3.
      *    05  PO-UPVB0092-VSWZ-Z-VERS-SUM      PIC S9(09)V99 COMP-3.
      *    05  PO-UPVB0092-VSWZ-G-VERS-NR       PIC S9(15)    COMP-3.
      *    05  PO-UPVB0092-VSWZ-Z-VERS-NR       PIC S9(15)    COMP-3.
      *    05  PO-UPVB0092-VSWZ-KNR-G-BERECHT   PIC S9(04)    COMP.
      *    05  PO-UPVB0092-VSWZ-ZUSAGE-DAT      PIC X(10).
      *    05  PO-UPVB0092-VSWZ-G-STZ-ANZ       PIC S9(04)    COMP.
      *    05  PO-UPVB0092-VSWZ-G-STZ-JAHR      PIC X(10).

           05  VSWZ-DATEN.
            10 PO-UPVB0092-VSWZ-LAST-UPDATE PIC X(26).
            10 PO-UPVB0092-VSWZ-ZUSAGE-DAT  PIC X(10).
            10 PO-UPVB0092-VSWZ-A-IG-DAT    PIC X(10).
            10 PO-UPVB0092-VSWZ-A-GESAMT    PIC S9(09)V99 COMP-3.
            10 PO-UPVB0092-VSWZ-G-DAT-VON   PIC X(10).
            10 PO-UPVB0092-VSWZ-G-STZ-JAHR  PIC X(10).
            10 PO-UPVB0092-VSWZ-G-VERS-SUM  PIC S9(09)V99 COMP-3.
            10 PO-UPVB0092-VSWZ-G-STZ-ANZ   PIC S9(4)     COMP.
            10 PO-UPVB0092-VSWZ-G-VERS-NR   PIC S9(15)V   COMP-3.
            10 PO-UPVB0092-VSWZ-Z-VERS-SUM  PIC S9(09)V99 COMP-3.
            10 PO-UPVB0092-VSWZ-Z-VERS-NR   PIC S9(15)V   COMP-3.
            10 PO-UPVB0092-VSWZ-Z-DAT-VON   PIC X(10).
            10 PO-UPVB0092-KTY-A-BERECHT    PIC S9(4)     COMP.
            10 PO-UPVB0092-KNR-A-BERECHT    PIC S9(4)     COMP.
            10 PO-UPVB0092-KTY-G-BERECHT    PIC S9(4)     COMP.
            10 PO-UPVB0092-KNR-G-BERECHT    PIC S9(4)     COMP.
            10 PO-UPVB0092-VSWZ-DBI-DAT     PIC X(10).
            10 PO-UPVB0092-VSWZ-DBI-KTO-NR  PIC X(9).



      ***** RESERVE
           05  FILLER                          PIC X(20).


      ******** ENDE COPY-MEMBER   DRVB0107   ***********************

