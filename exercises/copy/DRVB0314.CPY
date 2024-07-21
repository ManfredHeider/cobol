      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0314.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0314.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   03 Sep 1998 10:49:56   T13AE28
      *AVB00426 - Neue Funktion f. Window-Routine
      *
      *   Rev 1.0   09 Jul 1998 09:57:52   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0314                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0314                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    24.08.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.08.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0314
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von zwei 7stelligen Jahrszahlen des
      *            Formats 0JJMMTT in zwei 7stellige Jahreszahlen des
      *            Formates 0JJMMTT bei Jh.19  bzw. des
      *            Formates 1JJMMTT bei Jh.20
      *            mit fixer Grenze 80
      *                ----- ---------
      *
      *
      ****************************************************************
       01  PI-DRVB0314.
           05  PI-DRVB0314-JAHR-1             PIC  9(007).
      *
       01  PO-DRVB0314.
           05  PO-DRVB0314-RC                 PIC  X(003).
               88 PO-DRVB0314-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0314-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0314-RC-DAT-NOK                   VALUE 'DAT'.
               88 PO-DRVB0314-RC-1ST-NOK                   VALUE '1ST'.
           05  PO-DRVB0314-JAHR-1-7ST         PIC  9(007).
           05  PO-DRVB0314-JAHR-1-8ST.
            10 PO-DRVB0314-JAHR-1-8ST-JH      PIC  9(002).
            10 PO-DRVB0314-JAHR-1-8ST-JJ      PIC  9(002).
            10 PO-DRVB0314-JAHR-1-8ST-MM      PIC  9(002).
            10 PO-DRVB0314-JAHR-1-8ST-TT      PIC  9(002).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
