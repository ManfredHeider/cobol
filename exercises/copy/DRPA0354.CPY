      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0354.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0354.CPv  $
      *
      *   Rev 2.3   Aug 06 2015 10:05:34   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.2   Aug 06 2015 09:16:40   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Jul 16 2015 15:16:14   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.0   Feb 27 1999 14:28:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 16:21:56   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:30:40   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0354                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PDIF  >>                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    04.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.07.97!    ! NEU                       ! D.WILHELM*
      * APA01932!16.06.15!2.01! NEU:  UMSATZSTEUERBETRAG  ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0354                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0354.
          05 PI-DRPA0354-PROGRAMM            PIC  X(008).
          05 PI-DRPA0354-UFKT                PIC  9(002).
          05 PI-DRPA0354-SCHALTER            PIC  9.
          05 PI-DRPA0354-DATEN.
             10 PI-DRPA0354-PDTA-ID          PIC S9(015)  COMP-3.
      *
       01 PO-DRPA0354.
          05 PO-DRPA0354-RC                  PIC  X(003).
             88 PO-DRPA0354-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0354-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0354-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0354-DATEN.
             10 PO-DRPA0354-SCHALTER          PIC 9     VALUE ZERO.
             10 PO-DRPA0354-ZEILE-AUS OCCURS 2.
                15 PO-DRPA0354-PDIF-PGV            PIC S9(4)     COMP.
                15 PO-DRPA0354-PDIF-ANZ-BEWEGUNG   PIC S9(9)     COMP.
                15 PO-DRPA0354-PDIF-GUTSCHRIFTEN   PIC S9(09)V99 COMP-3.
                15 PO-DRPA0354-PDIF-BELASTUNGEN    PIC S9(09)V99 COMP-3.
160615          15 PO-DRPA0354-PDIF-UST-BETRAG-G   PIC S9(09)V99 COMP-3.
160615          15 PO-DRPA0354-PDIF-UST-BETRAG-B   PIC S9(09)V99 COMP-3.
 

