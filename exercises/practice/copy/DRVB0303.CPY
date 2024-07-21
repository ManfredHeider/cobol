      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0303.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0303.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jul 1998 16:21:24   T13AE28
      *AAT00004 - Y2K: Funktion fuer Window (UPVB0104)
      *
      *   Rev 1.0   09 Jul 1998 09:56:46   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0303                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0303                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                        *
      *    DATUM:    09.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !09.07.98! 01 ! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0303
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von einer 2stelligen Jahrszahl des
      *            Formats JJ in eine 4stellige Jahreszahl des
      *            Formates JJJJ mit fixer Grenze 70
      *                              ----- ---------
      *
      *
      ****************************************************************
       01  PI-DRVB0303.
           05  PI-DRVB0303-JAHR-1             PIC  9(002).
      *
       01  PO-DRVB0303.
           05  PO-DRVB0303-RC                 PIC  X(003).
               88 PO-DRVB0303-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0303-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0303-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0303-JAHR-1             PIC  9(004).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
