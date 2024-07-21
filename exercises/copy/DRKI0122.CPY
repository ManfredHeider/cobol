      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0122.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0122.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 May 1998 09:34:02   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:49:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
000100******************************************************************
000200*                                                                *
000300*    COPY-MEMBER ZU DEN AUFBEREITUNGSPROGRAMMEN KI-NEU           *
000400*     - Eingabedatei AMV-Kundendaten                         -   *
000500*                                                                *
000600*    SATZLÄNGE   300                                             *
000700*                                                                *
000800*    Anmerkung:                                                  *
000900*                                                                *
001000*                                                                *
001100*                                                                *
001200*                                                                *
001300*                                                                *
001400*                                                                *
001500*----------------------------------------------------------------*
001600*    LOGBUCH                                                     *
001700*DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
001800*AUFTRAG !                                       !         !     *
001900*--------!---------------------------------------!---------!-----*
002000*10.09.97!NEUERSTELLUNG                          !V.ZGUR-  ! 01  *
002100*KI-NEU  !                                       !VUJIC    !     *
002200*        !                                       !         !     *
002300*        !                                       !         !     *
002400******************************************************************
002500   03  amv-kunde.
002600
002700     05  amv-kun-aend-kz               pic 9(02).
002800     05  amv-kun-nach-name             pic x(40).
002810     05  amv-kun-vor-name              pic x(30).
002820     05  amv-kun-geb-datum.
002830        10  amv-kun-geb-jh             pic 9(02).
002840        10  amv-kun-geb-dat.
002850           15  amv-kun-geb-jj          pic 9(02).
002860           15  amv-kun-geb-mm          pic 9(02).
002870           15  amv-kun-geb-tt          pic 9(02).
002900     05  amv-kun-anrede-kz             pic 9(02).
002910     05  amv-kun-geschlecht            pic 9(01).
003000     05  amv-kun-strasse               pic x(45).
003010     05  amv-kun-postfach              pic x(45).
003020     05  amv-kun-post-lz               pic x(09).
003030     05  amv-kun-wohnort               pic x(40).
003040     05  amv-kun-ausl-kz               pic x(03).
003050     05  amv-kun-telefon-nr            pic x(15).
003060*       10  amv-kun-t-handy            pic x(04).
003070*       10  filler                     pic x(11).
003100     05  amv-kun-komm-kz               pic 9(02).
003200     05  amv-kun-vert-nr               pic 9(15).
003300     05  amv-kun-rolle-kz              pic 9(02).
003400     05  amv-kun-name-aend-kz          pic x(01).
003500     05  amv-kun-adress-aend-kz        pic x(01).
003600     05  amv-kun-sonst-aend-kz         pic x(01).
003700     05  amv-kun-res-feld              pic x(38).
