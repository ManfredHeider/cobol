      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0106.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0106.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Oct 30 1996 11:47:46   T13AE28
      *AFVB0316 Anpassungen/Korrekturen im Rahmen des VSW96
      *         D.Ehrmann
      *
      *   Rev 1.0   04 Oct 1996 12:20:48   P13SP17
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0106, SCHNITTSTELLE UPVB0091             *
      *                 ERMITTLUNG VSW-RELEVANTER VETRAGSDATEN       *
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
      *   PI-UPVB0091-FKT
      *      PI-UOVB0091-FKT-VSW-DATEN
      *         ...
      *
      *   PI-UPVB0091-VB-NR
      *         VB, FüR DEN DIE VERTRAGSDATEN ERMITTELT WERDEN SOLLEN
      *
      *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-UPVB0091.
           05  PI-UPVB0091-FKT                PIC  X(002).
               88 PI-UPVB0091-FKT-VERTR-DATEN      VALUE 'DA'.

      *    * üBERGEBENE DATEN
           05  PI-UPVB0091-VB-NR              PIC  9(007).
      *
      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-UPVB0091.

      *    * ERGEBNIS:
           05  PO-UPVB0091-RC                  PIC  X(003).
               88  PO-UPVB0091-RC-OK                VALUE 'OK '.
               88  PO-UPVB0091-RC-MA-FEHLER         VALUE '090'.
               88  PO-UPVB0091-RC-KEIN-AV-SATZ      VALUE '091'.
               88  PO-UPVB0091-RC-AV-FEHLER         VALUE '092'.
               88  PO-UPVB0091-RC-NOK               VALUE 'NOK'.

      *    * DATEN ZURÜCK
           05  PO-UPVB0091-ALT-VB-VERTRAG      PIC X(10).
           05  PO-UPVB0091-ALT-VERTRAG         PIC X(10).
           05  PO-UPVB0091-AKT-VB-VERTRAG      PIC X(10).
           05  PO-UPVB0091-BDV-MITGLIED        PIC X(01).
           05  FILLER                          PIC X(20).


      ******** ENDE COPY-MEMBER   DRVB0106   ***********************

