      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0290.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0290.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jul 1998 16:20:20   T13AE28
      *AAT00004 - Y2K: Funktion fuer Window (UPVB0104)
      *
      *   Rev 1.0   23 Jul 1997 12:27:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0290                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0290                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    23.06.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !23.06.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0290
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    Funktion:
      *            Umrechnen von 16 6stelligen Jahrszahlen des
      *            Formats JJMMTT in 16 8stellige Jahreszahlen des
      *            Formates JJJJMMTT mit fixer Grenze 70
      *                                  ----- ---------
      *
      *
      ****************************************************************
       01  PI-DRVB0290.
           05  PI-DRVB0290-JAHR-1             PIC  9(006).
           05  PI-DRVB0290-JAHR-2             PIC  9(006).
           05  PI-DRVB0290-JAHR-3             PIC  9(006).
           05  PI-DRVB0290-JAHR-4             PIC  9(006).
           05  PI-DRVB0290-JAHR-5             PIC  9(006).
           05  PI-DRVB0290-JAHR-6             PIC  9(006).
           05  PI-DRVB0290-JAHR-7             PIC  9(006).
           05  PI-DRVB0290-JAHR-8             PIC  9(006).
           05  PI-DRVB0290-JAHR-9             PIC  9(006).
           05  PI-DRVB0290-JAHR-10            PIC  9(006).
           05  PI-DRVB0290-JAHR-11            PIC  9(006).
           05  PI-DRVB0290-JAHR-12            PIC  9(006).
           05  PI-DRVB0290-JAHR-13            PIC  9(006).
           05  PI-DRVB0290-JAHR-14            PIC  9(006).
           05  PI-DRVB0290-JAHR-15            PIC  9(006).
           05  PI-DRVB0290-JAHR-16            PIC  9(006).
      *
       01  PO-DRVB0290.
           05  PO-DRVB0290-RC                 PIC  X(003).
               88 PO-DRVB0290-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0290-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0290-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0290-JAHR-1             PIC  9(008).
           05  PO-DRVB0290-JAHR-2             PIC  9(008).
           05  PO-DRVB0290-JAHR-3             PIC  9(008).
           05  PO-DRVB0290-JAHR-4             PIC  9(008).
           05  PO-DRVB0290-JAHR-5             PIC  9(008).
           05  PO-DRVB0290-JAHR-6             PIC  9(008).
           05  PO-DRVB0290-JAHR-7             PIC  9(008).
           05  PO-DRVB0290-JAHR-8             PIC  9(008).
           05  PO-DRVB0290-JAHR-9             PIC  9(008).
           05  PO-DRVB0290-JAHR-10            PIC  9(008).
           05  PO-DRVB0290-JAHR-11            PIC  9(008).
           05  PO-DRVB0290-JAHR-12            PIC  9(008).
           05  PO-DRVB0290-JAHR-13            PIC  9(008).
           05  PO-DRVB0290-JAHR-14            PIC  9(008).
           05  PO-DRVB0290-JAHR-15            PIC  9(008).
           05  PO-DRVB0290-JAHR-16            PIC  9(008).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
