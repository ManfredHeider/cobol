      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0359.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0359.CPv  $
      *
      *   Rev 2.3   Jul 22 2015 08:45:10   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      *Dialo
      *ganpassungen (PO26, PF12, PG13)
      *
      *   Rev 2.2   Jul 20 2015 16:11:12   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      *Dialo
      *ganpassungen (PO26, PF12, PG13)
      *
      *   Rev 2.1   Jul 16 2015 08:27:40   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      *Dialo
      *ganpassungen (PO26, PF12, PG13)
      *
      *   Rev 2.0   Feb 27 1999 14:28:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:22:32   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   Apr 09 1997 16:48:12   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0359                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0359                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    08.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0001!08.07.97! 01 ! NEU                       ! D.WILHELM*
      * APA01932!14.07.15! 01 ! NEUER PO-PARAMETER (LUCY) ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0359
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA0359 DES BUFFER-OZP ZYPA0004 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PG13, DTA Detailauskunft GeVo.
      *
      ****************************************************************
      *
       01  PI-DRPA0359.
           05  PI-DRPA0359-UFKT                PIC  9(02).
           05  PI-DRPA0359-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA0359-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA0359-PDTA-ID             PIC  S9(15) COMP-3.
      *
       01  PO-DRPA0359.
           05  PO-DRPA0359-RC                  PIC  X(03).
               88 PO-DRPA0359-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0359-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0359-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA0359-ANZAHL-ERG-ZEILEN   PIC  9(05).
           05  PO-DRPA0359-ERGEBNISZEILE.
               10 PO-DRPA0359-PGV                 PIC  S9(04) COMP.
               10 PO-DRPA0359-ANZAHL-V            PIC  S9(09) COMP.
               10 PO-DRPA0359-PG-GSMT-PROV-V      PIC  S9(09)V99
                                                      USAGE COMP-3.
140715         10 PO-DRPA0359-PG-UST-BETRAG-V     PIC  S9(09)V99
                                                      USAGE COMP-3.
               10 PO-DRPA0359-ANZAHL-NV           PIC  S9(09) COMP.
               10 PO-DRPA0359-PG-GSMT-PROV-NV     PIC  S9(09)V99
                                                      USAGE COMP-3.
140715         10 PO-DRPA0359-PG-UST-BETRAG-NV    PIC  S9(09)V99
                                                      USAGE COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

