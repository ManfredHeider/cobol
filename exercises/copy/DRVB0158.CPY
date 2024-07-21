      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0158.cpy  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\drvb0158.cpv  $
      *
      *   Rev 2.3   Jun 15 1999 13:53:18   T13AE42
      *AVB00445 - Delta-Rueckkonvertierung
      *
      *   Rev 2.2   Jun 09 1999 16:57:14   T13AE42
      *AVB00445 - Delta-Rueckkonvertierung
      *
      *   Rev 2.1   Jun 07 1999 14:52:32   T13AE42
      *AVB00445 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   May 14 1999 12:32:32   T13AE35
      *NT-Version
      *
      *   Rev 1.0   Feb 19 1997 09:59:30   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0158                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0158                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Maderner                                         *
      *    DATUM:    21.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0370!21.08.97! 01 ! NEU                       ! MADERNER *
      *         !21.10.97! 02 ! RAHMEN ANPASSEN           ! SCHMIDT  *
      * AVB00445!18.05.99! 03 ! TAGESDATUM-EUR            ! WALB     *
      * AVB00445!09.06.99! 04 ! ANPASSUNG SCHNITTSTELLE   ! SCHMIDT  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0158: ..
      *        -   -------------------
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************

       01 PI-DRVB0158.
211097*   05 PI-DRVB0158-ZEITPUNKT      PIC  9(06) COMP-3.
180599*   05 PI-DRVB0158-ZEITPUNKT-ISO  PIC  X(10).
090699*   05 PI-DRVB0158-TAGESDATUM     PIC  X(10).
090699    05 PI-DRVB0158-TAGESDATUM-EUR PIC  X(10).
090699    05 PI-DRVB0158-TAGESDATUM-ISO PIC  X(10).
211097    05 PI-DRVB0158-VBNR           PIC  9(07) COMP-3.
180599*211097    05 PI-DRVB0158-BETR-NR       PIC  9(07) COMP-3.
      *
       01  PO-DRVB0158.
           05  PO-DRVB0158-RC                  PIC  X(003).
               88 PO-DRVB0158-RC-OK                 VALUE 'OK '.
               88 PO-DRVB0158-RC-EOF                VALUE 'EOF'.
211097         88 PO-DRVB0158-RC-NOT-FOUND          VALUE 'NFD'.

           05  PO-DRVB0158-SATZ.

             10 PO-DRVB0158-VBNR    PIC 9(007)    OCCURS 20.
             10 PO-DRVB0158-DIR     PIC 999.
             10 PO-DRVB0158-STUFE   PIC 99        OCCURS 20.
      *
      ******* ENDE COPY-MEMBER DRVB0158 *****************************

