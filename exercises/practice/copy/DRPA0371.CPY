      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0371.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0371.CPv  $
      *
      *   Rev 2.3   Aug 06 2015 10:05:42   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.2   Aug 06 2015 09:16:54   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Jul 16 2015 15:16:32   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.0   Feb 27 1999 14:28:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:23:08   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:28:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0371                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PDIF  >>                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: E.SCHRIEBER                                      *
      *    DATUM:    21.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!21.07.97!    ! NEU                       ! SCHRIEBER*
      * APA01932!16.06.15!2.01! NEU:  UMSATZSTEUERBETRAG  ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0371                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0371.
          05 PI-DRPA0371-PROGRAMM            PIC  X(008).
          05 PI-DRPA0371-UFKT                PIC  9(02).
          05 PI-DRPA0371-DATEN.
             10 PI-DRPA0371-DTA-ID           PIC S9(015)  COMP-3.
             10 PI-DRPA0371-VSTAT-KLASSE     PIC  X(001).
      *
       01 PO-DRPA0371.
          05 PO-DRPA0371-RC                  PIC  X(003).
             88 PO-DRPA0371-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0371-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0371-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0371-DATEN.
             10 PO-DRPA0371-VSTAT                PIC  X(4).
             10 PO-DRPA0371-ANZAHL-BEWEGUNGEN    PIC S9(009)    COMP.
             10 PO-DRPA0371-SUMME-GUTSCHRIFTEN   PIC S9(009)V99 COMP-3.
             10 PO-DRPA0371-SUMME-BELASTUNGEN    PIC S9(009)V99 COMP-3.
160615       10 PO-DRPA0371-SUMME-UST-BETRAG-G   PIC S9(009)V99 COMP-3.
160615       10 PO-DRPA0371-SUMME-UST-BETRAG-B   PIC S9(009)V99 COMP-3.
      *
 

