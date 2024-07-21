      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0217.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0217.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:53:10   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0217                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0217                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Gsellhofer                                       *
      *    DATUM:    28.01.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY  1!28.01.97! 01 ! NEU                       ! Gsellh.  *
      *         !22.03.97!    ! Korrekturen               ! Auerbach *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0217
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0219 DES FÜHRT DEN UPDATE DER ANTRAGSHISTORIE
      *    aus
      *
      ****************************************************************
       01  PI-DRPA0217.
           05  C-MANR             PIC 9(7)    COMP-3.
           05  C-GES              PIC 99.
           05  C-VNR              PIC 9(15)   COMP-3.
           05  C-MANRN            PIC 9(7)    COMP-3.
           05  C-VMNR             PIC 9(7)    COMP-3.
           05  C-BETREUER         OCCURS 18.
               10 C-VBNR          PIC 9(7)    COMP-3.
               10 C-PRZ           PIC X.
               10 C-STRSTUFE      PIC 999     COMP-3.
               10 C-STRTEXT       PIC X(4).
               10 C-PROZ          PIC 9(4)V9  COMP-3.
           05  C-VNRN             PIC 9(15)   COMP-3.

           05  TS-USERID          PIC X(7).
           05  TS-DAT             PIC 9(7)    COMP-3.
           05  TS-UHR             PIC 9(9)    COMP-3.
      *
       01  PO-DRPA0217.
           05  PO-DRPA0217-RC                  PIC  X(03).
               88 PO-DRPA0217-RC-OK                        VALUE 'OK '.
