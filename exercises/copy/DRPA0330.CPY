      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0330.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0330.CPv  $
      *
      *   Rev 2.3   Aug 06 2015 09:54:30   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.2   Aug 06 2015 09:16:18   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Jul 16 2015 15:15:48   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.0   Feb 27 1999 14:28:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:54:46   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:38:26   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0330  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: OPRITZ                                          *
      *    DATUM:    09.06.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.06.97! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      *         !29.10.97!    ! NACHKOMMASTELLEN ANPASSEN ! MUND     *
      * APA01932!16.06.15!02.1! NEUES ATTRIBUT HINZU      ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0328                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE      TBPA0104 PDIF         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0330.
          05 PI-DRPA0330-PDIF-LAST-UPDATE     PIC  X(026).
          05 PI-DRPA0330-PMPG-KEY.
             10 PI-DRPA0330-PDTA-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0330-PDIF-PGV          PIC S9(004) COMP.
             10 PI-DRpa0330-PDIF-VSTAT        PIC  X(004).
          05 PI-DRPA0330-PDIF-ANZ-BEWEGUNG    PIC S9(009)      COMP.
          05 PI-DRPA0330-PDIF-GUTSCHRIFTEN    PIC S9(013)V9(2) COMP-3.
          05 PI-DRPA0330-PDIF-BELASTUNGEN     PIC S9(013)V9(2) COMP-3.
160615    05 PI-DRPA0330-PDIF-UST-BETRAG-G    PIC S9(009)V9(2) COMP-3.
160615    05 PI-DRPA0330-PDIF-UST-BETRAG-B    PIC S9(009)V9(2) COMP-3.
      *
       01 PO-DRPA0330.
          05 PO-DRPA0330-RC                   PIC  X(003).
             88 PO-DRPA0330-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0330-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0330-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************

