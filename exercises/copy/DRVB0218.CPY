      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0218.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0218.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jul 1998 16:06:40   T13AE28
      *AAT00004 - Funktion fuer Window (UPVB0104)
      *
      *   Rev 1.0   23 Jul 1997 12:17:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0218                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0218                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    23.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !23.03.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0218
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von zwei 2stelligen Jahrszahlen des
      *            Formats JJ in zwei 4stellige Jahreszahlen des
      *            Formates JJJJ mit sliding Grenze 80
      *                              ------- ---------
      ****************************************************************
      *
       01  PI-DRVB0218.
           05  PI-DRVB0218-JAHR-1             PIC  9(002).
           05  PI-DRVB0218-JAHR-2             PIC  9(002).
      *
       01  PO-DRVB0218.
           05  PO-DRVB0218-RC                 PIC  X(003).
               88 PO-DRVB0218-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0218-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0218-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0218-JAHR-1             PIC  9(004).
           05  PO-DRVB0218-JAHR-2             PIC  9(004).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
