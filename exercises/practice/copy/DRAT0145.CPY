      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0145.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRAT0145.CPv  $
      *
      *   Rev 2.1   12 Sep 2001 14:38:40   t13ae46
      *APA00144: Neu
      *
      *   Rev 2.0   Feb 27 1999 11:57:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Feb 19 1997 12:22:06   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0145                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPAT0034                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WANG                                             *
      *    DATUM:    16.08.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00144!16.08.01! 01 ! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01 PI-DRAT0145.
          05 PI-DRAT0145-JOBNAME                PIC X(008).
      *
       01 PO-DRAT0145.
          05 PO-DRAT0145-RC                     PIC  X(003).
             88 PO-DRAT0145-RC-OK                    VALUE 'OK '.
             88 PO-DRAT0145-RC-NOK                   VALUE 'NOK'.
      *
      ******* ENDE COPY-MEMBER **************************************
