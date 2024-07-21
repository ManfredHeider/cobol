      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0242.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0242.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jul 1998 16:17:22   T13AE28
      *AAT00004 - Y2K: Funktion fuer Window (UPVB0104)
      *
      *   Rev 1.0   23 Jul 1997 12:20:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0242                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0242                    *
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
      *    FÜR DIE FUNKTION DRVB0242
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von einer 4stelligen Jahrszahl des
      *            Formats JJMM in eine 6stellige Jahreszahl des
      *            Formates JJJJMM mit fixer Grenze 70
      *                                ----- ---------
      *
      *
      ****************************************************************
       01  PI-DRVB0242.
           05  PI-DRVB0242-JAHR-1             PIC  9(004).
      *
       01  PO-DRVB0242.
           05  PO-DRVB0242-RC                 PIC  X(003).
               88 PO-DRVB0242-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0242-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0242-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0242-JAHR-1             PIC  9(006).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
