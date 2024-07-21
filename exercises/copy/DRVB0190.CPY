      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0190.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drvb0190.cpv  $
      *
      *   Rev 2.2   Jun 15 1999 13:53:36   T13AE42
      *AVB00445 - Delta-Rueckkonvertierung
      *
      *   Rev 2.1   Jun 07 1999 14:52:54   T13AE42
      *AVB00445 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   May 14 1999 12:33:06   T13AE35
      *NT-Version
      *
      *   Rev 1.0   23 Jul 1997 12:13:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0190                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0190                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    01.12.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!01.12.97! 01 ! NEU                       ! SCHMIDT  *
      * AVB00445!18.05.99! 02 ! TAGESDATUM                ! WALB     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0190: ..
      *        -   -------------------
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************

       01 PI-DRVB0190.
      *   05 PI-DRVB0190-ZEITPUNKT     PIC  9(06) COMP-3.
180599*   05 PI-DRVB0190-ZEITPUNKT-ISO PIC  X(10).
180599    05 PI-DRVB0190-TAGESDATUM    PIC  X(10).
          05 PI-DRVB0190-VBNR          PIC  9(07) COMP-3.
180599*   05 PI-DRVB0190-BETR-NR       PIC  9(07) COMP-3.
      *
       01  PO-DRVB0190.
           05  PO-DRVB0190-RC                  PIC  X(003).
               88 PO-DRVB0190-RC-OK                 VALUE 'OK '.
               88 PO-DRVB0190-RC-EOF                VALUE 'EOF'.
               88 PO-DRVB0190-RC-NOT-FOUND          VALUE 'NFD'.

           05  PO-DRVB0190-SATZ.

             10 PO-DRVB0190-VBNR    PIC 9(007)    OCCURS 20.
             10 PO-DRVB0190-DIR     PIC 999.
             10 PO-DRVB0190-STUFE   PIC 99        OCCURS 20.
      *
      ******* ENDE COPY-MEMBER DRVB0190 *****************************

