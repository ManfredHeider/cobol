      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0440.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0440.CPv  $
      *
      *   Rev 2.3   Aug 06 2015 10:05:48   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.2   Aug 06 2015 09:17:22   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Jul 16 2015 15:17:08   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.0   12 Sep 2001 14:10:14   t13ae46
      *APA00144:Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:37:16   P13SP99
      *Initial revision.
      *
      ****************************************************************
      *    COPY-MEMBER: DRPA0440  FUNKTION ZU ZXPA0104               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J.WANG                                          *
      *    DATUM:    10.08.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00144!10.08.01! 1.0! NEUERSTELLUNG             ! J.WANG   *
      * APA01932!16.06.15!2.01! NEU:  UMSATZSTEUERBETRAG  ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0440                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE      TBPA0104 PDIF         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0440.
          05 PI-DRPA0440-PROGRAMM            PIC  X(008).
          05 PI-DRPA0440-UFKT                PIC  9(002).
          05 PI-DRPA0440-DATEN.
             10 PI-DRPA0440-PDTA-ID          PIC S9(015) COMP-3.
             10 PI-DRPA0440-PGEB-GES         PIC S9(004) BINARY.
      *
       01 PO-DRPA0440.
          05 PO-DRPA0440-RC                   PIC  X(003).
             88 PO-DRPA0440-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0440-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA0440-RC-EOF                        VALUE 'EOF'.
      *
          05 PO-DRPA0440-PDTA-ID              PIC S9(015) COMP-3.
          05 PO-DRPA0440-PDIF-PGV             PIC S9(004) COMP.
          05 PO-DRPA0440-PDIF-VSTAT           PIC  X(004).
          05 PO-DRPA0440-PDIF-ANZ-BEWEGUNG    PIC S9(009)      COMP.
          05 PO-DRPA0440-PDIF-GUTSCHRIFTEN    PIC S9(013)V9(2) COMP-3.
          05 PO-DRPA0440-PDIF-BELASTUNGEN     PIC S9(013)V9(2) COMP-3.
          05 PO-DRPA0440-PDIF-LAST-UPDATE     PIC  X(026).
          05 PO-DRPA0440-PDIF-KENNUNG         PIC  X(008).
160615    05 PO-DRPA0440-PDIF-UST-BETRAG-G    PIC S9(009)V9(2) COMP-3.
160615    05 PO-DRPA0440-PDIF-UST-BETRAG-B    PIC S9(009)V9(2) COMP-3.
      *
      ****** ENDE COPY-MEMBER ****************************************
 

