      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0332.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0332.CPv  $
      *
      *   Rev 2.1   Jan 11 2017 10:08:38   t13ae31
      *AKI01475: Partnertexte angleichen DB2
      *
      *   Rev 2.0   Feb 27 1999 12:50:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:20:44   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:18   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AFKI0000 V005.000 19980114'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  PARTNERGESELLSCHAF  TABLE
CCC004*    ( PARTGESELLSCHAFTID      SMALLINT NOT NULL,
CCC004*      PGESGRUPPE              SMALLINT NOT NULL,
CCC004*      PARTGESELLSCHNAME       CHAR (25) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  PARTNERGESELLSCHAF-GESAMT.
        05  NI-PARTNERGESELLSCHAF-TAB.
           10 NI-PARTNERGESELLSCHAF-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05  NI-PARTNERGESELLSCHAF  REDEFINES  NI-PARTNERGESELLSCHAF-TAB.
           10 NI-PARTGESELLSCHAFTID       PIC S9(4) COMP.
           10 NI-PGESGRUPPE               PIC S9(4) COMP.
           10 NI-PARTGESELLSCHNAME        PIC S9(4) COMP.
        05  DCL-PARTNERGESELLSCHAF.
           10 PARTGESELLSCHAFTID          PIC S9(4) COMP.
           10 PGESGRUPPE                  PIC S9(4) COMP.
           10 PARTGESELLSCHNAME           PIC X(60).
      ******************************************************************

