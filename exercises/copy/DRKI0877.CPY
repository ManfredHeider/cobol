      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0877.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0877.cpv  $
      *
      *   Rev 2.1   Jun 28 1999 12:59:18   T13EX19
      *fki00070 - neueerstellung
      *
      *   Rev 2.0   Apr 29 1999 13:57:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00070!24.06.99!    ! neue fkt DRKI0877         ! Zgur-Vujic
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  PI-DRKI0877.
           05  PI-DRKI0877-KEY.


             10 PI-DRKI0877-ADMAPERSONENID   PIC X(13).
             10 PI-DRKI0877-ADMAPERSONENROLLE   PIC X(01).


           05  PI-DRKI0877-DATEN.


             10 PI-DRKI0877-ADMASPERRE         PIC X(01).


      *
       01  PO-DRKI0877.
           05  PO-DRKI0877-RC                 PIC  X(003).
               88 PO-DRKI0877-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0877-RC-NOT-FOUND                 VALUE 'NFD'.
      *

