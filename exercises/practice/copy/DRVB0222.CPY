      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0222.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0222.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jul 1998 16:08:56   T13AE28
      *AAT00004 - Y2K: Funktion fuer Window (UPVB0104)
      *
      *   Rev 1.0   23 Jul 1997 12:18:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0222                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0222                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    23.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !07.04.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0222
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von einer zweistelligen Jahrszahl des
      *            Formats JJ in eine 4stellige Jahreszahl des
      *            Formates JJJJ mit sliding Grenze 80
      *                              ------- ---------
      ****************************************************************
      *
       01  PI-DRVB0222.
           05  PI-DRVB0222-JAHR-1             PIC  9(002).
      *
       01  PO-DRVB0222.
           05  PO-DRVB0222-RC                 PIC  X(003).
               88 PO-DRVB0222-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0222-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0222-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0222-JAHR-1             PIC  9(004).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
