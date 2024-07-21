      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0232.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0232.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jul 1998 16:14:04   T13AE28
      *AAT00004 - Y2K: Funktion fuer Window (UPVB0104)
      *
      *   Rev 1.0   23 Jul 1997 12:19:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0232                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0232                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    04.05.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !04.05.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0232
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von drei 6stelligen Jahrszahlen des
      *            Formats JJMMTT in drei 8stellige Jahreszahlen des
      *            Formates JJJJMMTT mit fixer Grenze 70
      *                                  ----- ---------
      *
      *
      ****************************************************************
       01  PI-DRVB0232.
           05  PI-DRVB0232-JAHR-1             PIC  9(006).
           05  PI-DRVB0232-JAHR-2             PIC  9(006).
           05  PI-DRVB0232-JAHR-3             PIC  9(006).
      *
       01  PO-DRVB0232.
           05  PO-DRVB0232-RC                 PIC  X(003).
               88 PO-DRVB0232-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0232-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0232-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0232-JAHR-1             PIC  9(008).
           05  PO-DRVB0232-JAHR-2             PIC  9(008).
           05  PO-DRVB0232-JAHR-3             PIC  9(008).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
