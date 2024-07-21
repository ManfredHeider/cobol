      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0235.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0235.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jul 1998 16:15:46   T13AE28
      *AAT00004 - Y2K: Funktion fuer Window (UPVB0104)
      *
      *   Rev 1.0   23 Jul 1997 12:19:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0235                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0235                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    07.05.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !07.05.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0235
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von einer 6stelligen Jahrszahl des
      *            Formats JJMMTT in eine 8stellige Jahreszahl des
      *            Formates JJJJMMTT mit fixer Grenze 70
      *                                  ----- ---------
      *
      *
      ****************************************************************
       01  PI-DRVB0235.
           05  PI-DRVB0235-JAHR-1             PIC  9(006).
      *
       01  PO-DRVB0235.
           05  PO-DRVB0235-RC                 PIC  X(003).
               88 PO-DRVB0235-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0235-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0235-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0235-JAHR-1             PIC  9(008).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
