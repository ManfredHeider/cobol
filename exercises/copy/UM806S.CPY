      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UM806S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UM806S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:01:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:36:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION  1
      ******************************************************************
      * COPYSTRECKE FUER DIE DATENKOMPRIMIERUNG               19.02.90 *
      ******************************************************************
      * IN DAS FELD COMP-DATENBEREICH WERDEN DIE ZU KOMPRIMIERENDEN    *
      * DATEN EINGESTELLT.  IM FELD COMP-DATENBEREICH-LAENGE MUß       *
      * ANGEGEBEN WERDEN, WIEVIEL  BYTES EINGESTELLT WURDEN.           *
      *                                                                *
      * WIRD DAS UP ZUM ERSTEN MAL AUFGERUFEN, IST DAS FELD            *
      * 'START-COMPRESS-FIELD' MIT EINER '1' ZU BELEGEN.              *
      * (BEI FOLGEAUFRUFEN IST DARAUF ZU ACHTEN, DAß DAS FELD AUF '0'  *
      *  GESETZT WIRD).                                                *
      * WERDEN NOCH WEITERE BLÖCKE ÜBERGEBEN, SO IST DAS FELD          *
      * 'END-COMPRESS-FIELD' AUF VALUE '0' ZU SETZEN.                  *
      * ERST BEI DER ÜBERGABE DES LETZTEN DATENBLOCKS IST DER INHALT   *
      * DIESER VARIABLEN AUF '1' ZU SETZEN.                            *
      ******************************************************************
      * DAS FELD 'FILE-NAMEN' IST GLEICHBEDEUTEND MIT DEM FELD         *
      * 'FL-NAMEN' AUS FILELINK, UND BEIM LETZTEN AUFRUF ZU FUELLEN    *
      ******************************************************************
      * DAS FELD 'WHAT-TO-DO'  IST MIT EINEM ENTSPRECHENDEN WERT ZU    *
      * BELEGEN                                                        *
      ******************************************************************


      *01 COMPRESS-VARS.
           03  COMP-DATENBEREICH.
               05 COMP-BYTE                PIC X   OCCURS 10000.
           03  COMP-DATENBEREICH-LAENGE    PIC 9(0005).
           03  START-COMPRESS-FIELD         PIC 9.
               88 START-COMPRESS        VALUE 1.
           03  END-COMPRESS-FIELD          PIC 9.
               88 END-COMPRESS         VALUE 1.
           03  WHAT-TO-DO                   PIC 9.
               88 DO-COMPRESS          VALUE 1.
               88 DO-TRANSLATE         VALUE 2.
               88 DO-BOTH              VALUE 3.
           03  FILE-NAMEN                  PIC X(0075).


