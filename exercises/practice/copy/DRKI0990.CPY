      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0990.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0990.CPv  $
      *
      *   Rev 2.1   Mar 03 2000 14:44:24   T13EX40
      *AKI00016 - EA-Verfahren
      *
      *   Rev 2.0   24 Jan 2000 16:35:56   P13SP99
      *Neuanlage
      *
      *---------------------------------------------------------------*
      ***************************************************************
       01  PI-DRKI0990.
           05  PI-DRKI0990-TAB-CNT           PIC S9(09) COMP.
           05  PI-DRKI0990-TAB            OCCURS 999.
               10 PI-DRKI0990-ZIEL-PC        PIC S9(09) COMP.

      *
       01  PO-DRKI0990.
           05  PO-DRKI0990-TAB-CNT           PIC S9(09) COMP.
           05  PO-DRKI0990-TAB            OCCURS 999.
               10 PO-DRKI0990-PC-FLAG        PIC X(01).
                  88 PO-DRKI0990-PC-OK                  VALUE 'J'.
                  88 PO-DRKI0990-PC-FAILED              VALUE 'N'.

               10 PO-DRKI0990-ZIEL-PC           PIC S9(09) COMP.
               10 PO-DRKI0990-UNIQUE-ID.
                  15 PO-DRKI0990-UNIQUE-ID-LEN  PIC S9(04) COMP.
                  15 PO-DRKI0990-UNIQUE-ID-TEXT PIC  X(14).
               10 PO-DRKI0990-PROXY-NR          PIC S9(09) COMP.

           05  PO-DRKI0990-RETRY-FLAG         PIC  X(001).
               88 PO-DRKI0990-RETRY                     VALUE 'J'.
               88 PO-DRKI0990-NORETRY                   VALUE 'N'.

           05  PO-DRKI0990-RC                 PIC  X(003).
               88 PO-DRKI0990-RC-OK                      VALUE 'OK '.
               88 PO-DRKI0990-RC-NOK                     VALUE 'NOK'.
      *
      ******* ENDE COPY-MEMBER **************************************           
