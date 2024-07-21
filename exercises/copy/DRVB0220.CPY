      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0220.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0220.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jul 1998 16:07:24   T13AE28
      *AAT00004 - Y2K: Funktion fuer Window (UPVB0104)
      *
      *   Rev 1.0   23 Jul 1997 12:17:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0220                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0220                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    23.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.03.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0220
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von zwei 6stelligen Jahrszahlen des
      *            Formats JJMMTT in zwei 8stellige Jahreszahlen des
      *            Formates JJJJMMTT mit sliding Grenze 80
      *                                  ------- ---------
      *
      *
      ****************************************************************
       01  PI-DRVB0220.
           05  PI-DRVB0220-JAHR-1             PIC  9(006).
           05  PI-DRVB0220-JAHR-2             PIC  9(006).
      *
       01  PO-DRVB0220.
           05  PO-DRVB0220-RC                 PIC  X(003).
               88 PO-DRVB0220-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0220-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0220-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0220-JAHR-1             PIC  9(008).
           05  PO-DRVB0220-JAHR-2             PIC  9(008).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
