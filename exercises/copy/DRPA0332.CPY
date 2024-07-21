      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0332.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0332.CPv  $
      *
      *   Rev 2.3   Aug 06 2015 09:54:36   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.2   Aug 06 2015 09:16:26   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Jul 16 2015 15:15:56   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.0   Feb 27 1999 14:28:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:55:58   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:39:30   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0332  FUNKTION SELECT STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0332                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE      TBPA0104 PDIF         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0332.
          05 PI-DRPA0332-PMPG-KEY.
             10 PI-DRPA0332-PDTA-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0332-PDIF-PGV          PIC S9(004) COMP.
             10 PI-DRPA0332-PDIF-VSTAT        PIC  X(004).
      *
       01 PO-DRPA0332.
          05 PO-DRPA0332-RC                   PIC  X(003).
             88 PO-DRPA0332-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0332-RC-NOT-FOUND                  VALUE 'NFD'.
      *
          05 PO-DRPA0332-PDIF-ANZ-BEWEGUNG    PIC S9(009)      COMP.
          05 PO-DRPA0332-PDIF-GUTSCHRIFTEN    PIC S9(013)V9(2) COMP-3.
          05 PO-DRPA0332-PDIF-BELASTUNGEN     PIC S9(013)V9(2) COMP-3.
          05 PO-DRPA0332-PDIF-LAST-UPDATE     PIC  X(026).
          05 PO-DRPA0332-PDIF-KENNUNG         PIC  X(008).
160615    05 PO-DRPA0332-PDIF-UST-BETRAG-G    PIC S9(009)V9(2) COMP-3.
160615    05 PO-DRPA0332-PDIF-UST-BETRAG-B    PIC S9(009)V9(2) COMP-3.
      *
      ****** Ende Copy-Member ****************************************

