      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0243.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0243.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jul 1998 16:17:54   T13AE28
      *AAT00004 - Y2K: Funktion fuer Window (UPVB0104)
      *
      *   Rev 1.0   23 Jul 1997 12:21:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0243                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0243                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    14.05.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !14.05.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0243
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von zwei 4stelligen Jahrszahlen des
      *            Formats JJMM in zwei 6stellige Jahreszahlen des
      *            Formates JJJJMM mit fixer Grenze 70
      *                                ----- ---------
      *
      *
      ****************************************************************
       01  PI-DRVB0243.
           05  PI-DRVB0243-JAHR-1             PIC  9(004).
           05  PI-DRVB0243-JAHR-2             PIC  9(004).
      *
       01  PO-DRVB0243.
           05  PO-DRVB0243-RC                 PIC  X(003).
               88 PO-DRVB0243-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0243-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0243-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0243-JAHR-1             PIC  9(006).
           05  PO-DRVB0243-JAHR-2             PIC  9(006).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
