      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU540S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU540S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Dec 1998 08:41:16   T13EX36
      *AOP00161 - Jahr2000-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:01:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ****************************************************************
      *                                                              *
      *          DATEI FUER DIE PRODUKTENTABELLE                     *
      *          FESTE LAENGE; LAENGE =  59  BYTES                   *
      *          SCHLUESSEL: TKEY                                    *
      *          SATZART   : 54                                      *
      *          COPYMEMBER: UU540S                                  *
      *                                                              *
      *          SATZLäNGE GEäNDERT   =  67  BYTES SL                *
      *          Y2K 4 * JH IM SCHLüSSEL    KüHBACHER  04.08.98      *
      ****************************************************************
       01  PRODTAB.
           03 TSA                       PIC XX.
           03 TKEY.
              05  TGES                  PIC 99.
              05  TPRODSCHL             PIC 99.
*Y2K**        05  TBEGINN-JH            PIC 99.
              05  TBEGINN               PIC 9(6).
*Y2K**        05  TABRECHDAT-JH         PIC 99.
              05  TABRECHDAT            PIC 9(6).
*Y2K**        05  TFAELLIG-JH           PIC 99.
              05  TFAELLIG              PIC 9(6).
*Y2K**        05  TRESDAT-JH            PIC 99.
              05  TRESDAT               PIC 9(6).
           03 TDATEN.
              05  TGRUND                PIC 9(4)V99.
              05  TRUECK                PIC 99.
              05  TVERT                 PIC 9(4)V99.
              05  TCOVER                PIC 9(4)V99.
              05  TPROVPFL              PIC 9(7)V99.

