      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0630.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0630.CPv  $
      *
      *   Rev 2.3   Aug 06 2015 10:05:56   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.2   Aug 06 2015 09:17:48   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Jul 16 2015 15:17:36   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.0   Feb 27 1999 14:29:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:18:48   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:56:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0630  FUNKTION ZU ZXPA0104               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                       *
      *    DATUM:    18.11.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!18.11.97! 01 ! NEUERSTELLUNG             ! D.WILHELM*
      * APA01932!16.06.15!2.01! NEU:  UMSATZSTEUERBETRAG  ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0630                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE      TBPA0104 PDIF         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0630.
          05 PI-DRPA0630-PROGRAMM            PIC  X(008).
          05 PI-DRPA0630-UFKT                PIC  9(002).
          05 PI-DRPA0630-PMPG-KEY.
             10 PI-DRPA0630-PDTA-ID          PIC S9(015)  COMP-3.
             10 PI-DRPA0630-VSTAT-KLASSE     PIC  X(001).
      *
       01 PO-DRPA0630.
          05 PO-DRPA0630-RC                   PIC  X(003).
             88 PO-DRPA0630-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0630-RC-NOT-FOUND                  VALUE 'NFD'.
             88 PO-DRPA0630-RC-EOF                        VALUE 'EOF'.
      *
          05 PO-DRPA0630-PDIF-PGV             PIC S9(004) COMP.
          05 PO-DRPA0630-PDIF-VSTAT           PIC  X(004).
          05 PO-DRPA0630-PDIF-ANZ-BEWEGUNG    PIC S9(009)      COMP.
          05 PO-DRPA0630-PDIF-GUTSCHRIFTEN    PIC S9(013)V9(2) COMP-3.
          05 PO-DRPA0630-PDIF-BELASTUNGEN     PIC S9(013)V9(2) COMP-3.
          05 PO-DRPA0630-PDIF-LAST-UPDATE     PIC  X(026).
          05 PO-DRPA0630-PDIF-KENNUNG         PIC  X(008).
160615    05 PO-DRPA0630-PDIF-UST-BETRAG-G    PIC S9(009)V9(2) COMP-3.
160615    05 PO-DRPA0630-PDIF-UST-BETRAG-B    PIC S9(009)V9(2) COMP-3.
      *
      ****** Ende Copy-Member ****************************************
 

