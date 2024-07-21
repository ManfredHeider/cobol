      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1008.CPY  $
      *    $Revision:   1.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1008.CPv  $
      *
      *   Rev 1.3   Aug 06 2015 09:17:56   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 1.2   Jul 16 2015 15:17:46   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 1.1   12 Sep 2001 14:29:20   t13ae46
      *APA00144: Neue : K.INSERT-SICHERUNG
      *
      *   Rev 1.0   08 Jul 1998 08:51:46   P13SP19
      *Initial revision.
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1008                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE 'DTA SICHERN' FÜR                            *
      *       PROGRAMM UPPA0083 (DTA-SACHBEARBEITER)                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. WANG                                          *
      *    DATUM:    28.08.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00144!28.08.01! 01 ! NEUERSTELLUNG             ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *
      ****************************************************************
      *
       01 PI-DRPA1008.
          05 PI-DRPA1008-PDTA-ID                 PIC S9(015)V COMP-3.
          05 PI-DRPA1008-PGEB-GES                PIC S9(04) USAGE COMP.
      *
       01 PO-DRPA1008.
          05 PO-DRPA1008-DATEN.
             10 PO-DRPA1008-PDIF-ZAEHLER PIC S9(009) BINARY.
             10 PO-DRPA1008-PMPE-ZAEHLER PIC S9(009) BINARY.
             10 PO-DRPA1008-PMPP-ZAEHLER PIC S9(009) BINARY.
             10 PO-DRPA1008-GSMT-PROV   PIC S9(009)V9(2) PACKED-DECIMAL.
          05 PO-DRPA1008-RC                      PIC  X(003).
             88 PO-DRPA1008-RC-OK                     VALUE 'OK '.
      ******* ENDE COPY-MEMBER **************************************
      *---------------------------------------------------------------*
 

