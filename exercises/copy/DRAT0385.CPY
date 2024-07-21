      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0385.CPY  $

      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAT0385.CPv  $
      *
      *   Rev 2.4   Jun 01 2017 15:07:02   t13ae13
      *ASL00567: LUCY2: SL10 (QED-Provision) anpassen (SWL)
      *
      *   Rev 2.3   Jul 19 2004 10:19:06   t13ex22
      *ASL00183: MAP - Manageraufbauplan; Dialog SL02
      *
      *   Rev 2.2   Dec 17 2002 11:56:18   t13ae35
      *ASL00153 - neuer Grund=4 eingefügt
      *
      *   Rev 2.1   Dec 10 2002 15:18:40   t13ae35
      *ASL00153 - neu
      *
      *   Rev 2.0   26 Jun 2001 12:55:08   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0324  KONSTANTEN FÜR KTY = 80            *
      *                           Anspruchsgrund (Sonderleistungen)  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. Hering                                       *
      *    DATUM:    27.11.2002                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00153!27.11.02!2.1 ! NEU                       ! J.Hering *
      * ASL00153!17.12.02!2.2 ! neuen Grund=4 eingefügt   ! J.Hering *
      * ASL00183!19.07.04!2.3 ! NEUEN GRUND 5 EINGEFÜGT   ! T.MUND   *
      * ASL00567!28.04.17!2.4 ! weitere Gründe eingefügt  ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-MASCHINELL-ERM        PIC S9(004) BINARY VALUE   1.
           05 K-KNR-MANUELL-ED            PIC S9(004) BINARY VALUE   2.
           05 K-KNR-MANUELL-KULANZ        PIC S9(004) BINARY VALUE   3.
           05 K-KNR-MANUELL-KORREKTUR     PIC S9(004) BINARY VALUE   4.
           05 K-KNR-MANUELL-MAP           PIC S9(004) BINARY VALUE   5.
280417     05 K-KNR-MASCHINELL-RUECKBEL   PIC S9(004) BINARY VALUE   6.
280417     05 K-KNR-MANUELL-RUECKBEL      PIC S9(004) BINARY VALUE   7.
280417     05 K-KNR-MASCHINELL-RUECKBEL-2 PIC S9(004) BINARY VALUE   8.
280417     05 K-KNR-MANUELL-RUECKBEL-2    PIC S9(004) BINARY VALUE   9.
280417     05 K-KNR-MANUELL-KORR-1        PIC S9(004) BINARY VALUE  41.
280417     05 K-KNR-MANUELL-KORR-2        PIC S9(004) BINARY VALUE  42.
280417     05 K-KNR-MANUELL-KORR-3        PIC S9(004) BINARY VALUE  43.
280417     05 K-KNR-MANUELL-KORR-4        PIC S9(004) BINARY VALUE  44.
280417     05 K-KNR-MANUELL-KORR-5        PIC S9(004) BINARY VALUE  45.

