      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0496.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0496.CPv  $
      *
      *   Rev 2.0   May 27 1999 11:42:58   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:43:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0496                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0496                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    10.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.09.97! 01 ! NEU                       ! D.WILHELM*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0496
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0496 DES BUFFER-OZP ZYPA0004 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PG18, DTA freigeben / sperren.
      *
      ****************************************************************
      *
       01  PI-DRPA0496.
           05  PI-DRPA0496-UFKT                PIC  9(02).
           05  PI-DRPA0496-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0496-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA0496-PDTA-ID             PIC  9(15).
      *
       01  PO-DRPA0496.
           05  PO-DRPA0496-RC                  PIC  X(03).
               88 PO-DRPA0496-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0496-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0496-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0496-ANZAHL-ERG-ZEILEN   PIC  9(05).
           05  PO-DRPA0496-ERGEBNISZEILE.
               10 PO-DRPA0496-ANZAHL-V            PIC  9(06).
               10 PO-DRPA0496-PROVBETRAG-V        PIC  S9(15)V99.
               10 PO-DRPA0496-ANZAHL-NV           PIC  9(06).
               10 PO-DRPA0496-PROVBETRAG-NV       PIC  S9(15)V99.
      *
      ******* ENDE COPY-MEMBER **************************************
