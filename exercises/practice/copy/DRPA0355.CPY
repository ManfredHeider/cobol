      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0355.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0355.CPv  $
      *
      *   Rev 2.3   Aug 06 2015 10:12:14   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.2   Aug 06 2015 09:16:46   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Jul 16 2015 15:16:24   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.0   Feb 27 1999 14:28:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 16:22:28   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:30:40   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0355                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PDIF  >>                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    07.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!07.07.97!    ! NEU                       ! D.WILHELM*
      * APA01932!16.06.15!2.01! NEU:  UMSATZSTEUERBETRAG  ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0355                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0355.
          05 PI-DRPA0355-PROGRAMM            PIC  X(008).
          05 PI-DRPA0355-UFKT                PIC  9(002).
          05 PI-DRPA0355-DATEN.
             10 PI-DRPA0355-PDTA-ID          PIC S9(015)  COMP-3.
      *
       01 PO-DRPA0355.
          05 PO-DRPA0355-RC                  PIC  X(003).
             88 PO-DRPA0355-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0355-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0355-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0355-DATEN.
             10 PO-DRPA0355-PDIF-PGV            PIC S9(004)    COMP.
             10 PO-DRPA0355-PDIF-ANZ-BEWEGUNG   PIC S9(009)    COMP.
             10 PO-DRPA0355-PDIF-GUTSCHRIFTEN   PIC S9(009)V99 COMP-3.
             10 PO-DRPA0355-PDIF-BELASTUNGEN    PIC S9(009)V99 COMP-3.
160615       10 PO-DRPA0355-PDIF-UST-BETRAG-G   PIC S9(009)V99 COMP-3.
160615       10 PO-DRPA0355-PDIF-UST-BETRAG-B   PIC S9(009)V99 COMP-3.
      *
 

