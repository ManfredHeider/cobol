      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0228.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0228.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jul 1998 16:12:44   T13AE28
      *AAT00004 - Y2K: Funktion fuer Window (UPVB0104)
      *
      *   Rev 1.0   23 Jul 1997 12:19:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0228                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0228                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    23.04.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.04.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0228
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von zwei 2stelligen Jahrszahlen des
      *            Formats JJ in zwei 4stellige Jahreszahlen des
      *            Formates JJJJ mit fixer Grenze 80
      *                              ----- ---------
      *
      *
      ****************************************************************
       01  PI-DRVB0228.
           05  PI-DRVB0228-JAHR-1             PIC  9(002).
           05  PI-DRVB0228-JAHR-2             PIC  9(002).
      *
       01  PO-DRVB0228.
           05  PO-DRVB0228-RC                 PIC  X(003).
               88 PO-DRVB0228-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0228-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0228-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0228-JAHR-1             PIC  9(004).
           05  PO-DRVB0228-JAHR-2             PIC  9(004).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
