      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0288.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0288.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jul 1998 16:19:44   T13AE28
      *AAT00004 - Y2K: Funktion fuer Window (UPVB0104)
      *
      *   Rev 1.0   23 Jul 1997 12:26:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0288                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0288                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    22.06.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !22.06.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0288
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von einer 4stellige Jahrszahl des
      *            Formats JJMM in eine 6stellige Jahreszahl des
      *            Formates JJJJMM mit fixer Grenze 80
      *                                ----- ---------
      *
      *
      ****************************************************************
       01  PI-DRVB0288.
           05  PI-DRVB0288-JAHR-1             PIC  9(004).
      *
       01  PO-DRVB0288.
           05  PO-DRVB0288-RC                 PIC  X(003).
               88 PO-DRVB0288-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0288-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0288-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0288-JAHR-1             PIC  9(006).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
