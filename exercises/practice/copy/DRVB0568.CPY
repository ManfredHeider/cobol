      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0568.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRVB0568.CPv  $
      *
      *   Rev 2.2   Feb 20 2003 14:42:50   t13ex22
      *AAN00660: Erfassen eMail im VB43
      *
      *   Rev 2.1   Aug 09 2002 14:10:28   t13ae27
      *EMAIL/URL-Einführung
      *
      *   Rev 2.0   15 May 2001 13:33:46   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0567, SCHNITTSTELLE UPVB0093             *
      *                 ERM. KORRIGIERTE EINHEITEN UND UNTST-INFOS   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: W. SCH.LLHAMMER                                 *
      *    DATUM:    09.09.1996                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00587!26.02.02! 1  ! neu                       ! P.EDuerr *
      * AVB00660!12.02.02! 2  ! NEUE PRÜFROUTINE          ! T.MUND   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-UPVB0123.
           05  PI-UPVB0123-MA-ST-NR           PIC S9(009) COMP.
           05  PI-UPVB0123-ANW-NR             PIC S9(004) COMP.
               88  PI-UPVB0123-EMAIL                       VALUE 1.
               88  PI-UPVB0123-URL                         VALUE 2.
120203         88  PI-UPVB0123-NAME                        VALUE 3.
           05  PI-UPVB0123-KZ-NAME-VORNAME    PIC X(001).
           05  PI-UPVB0123-STRING             PIC X(070).
      *
      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-UPVB0123.

      *    * ERGEBNIS:
           05  PO-UPVB0123-RC                 PIC  X(003).
               88  PO-UPVB0123-RC-OK                       VALUE 'OK '.
               88  PO-UPVB0123-RC-FEHLER                   VALUE 'FE '.
               88  PO-UPVB0123-RC-ERR                      VALUE 'ERR'.

      ****** INFORMATIONEN ZU NAME

           05  PO-UPVB0123-STRING             PIC X(070).
           05  PO-UPVB0123-BEMERKUNG          PIC X(050).
      *
      *----------------------------------------------------------------
