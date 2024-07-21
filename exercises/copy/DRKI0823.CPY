      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0823.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0823.CPv  $
      *
      *   Rev 2.1   Apr 19 1999 11:35:48   T13EX40
      * 
      *
      *   Rev 2.0   Feb 27 1999 12:58:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:49:24   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
       01  PI-DRKI0823.                                                         
           05  PI-DRKI0823-UFKT             PIC XX.
               88 ASCII-EBCDIC                  VALUE 'AE'.
               88 EBCDIC-ASCII                  VALUE 'EA'.

           05 PI-DRKI0823-LEN               PIC 9(5) COMP.                      
           05 PI-DRKI0823-BUF               PIC X(66000).                      
                                                                                
       01  PO-DRKI0823.                                                         
           05  PO-DRKI0823-RC               PIC  X(003).
               88 PO-DRKI0823-RC-OK              VALUE 'OK '.
               88 PO-DRKI0823-RC-NOK             VALUE 'NOK'.
           05 PO-DRKI0823-LEN               PIC 9(5) COMP.                      
           05 PO-DRKI0823-BUF               PIC X(66000).
