CREATE DATEBASE 'localhost:C:\DB\TimeTable\TEST.FDB' user 'SYSDBA' password 'masterserver';

CREATE TABLE teachers(
	id integer primary key,
	name varchar(50)
	);

CREATE TABLE subjects(
	id integer primary key,
	name varchar(50)
	);

CREATE TABLE groups(
	id integer primary key,
	name varchar(50)
	);
	
CREATE TABLE classrooms(
	id integer primary key,
	name varchar(50)
	);

CREATE TABLE teachers_subjects(
	teacher_id integer,
	subject_id integer
	);
	
CREATE TABLE groups_subjects(
	group_id integer,
	subject_id integer
	);
	
CREATE TABLE weekday (
	id integer primary key,
	Weekday varchar(15)
	);

CREATE TABLE pair(
	id integer primary key,
	num integer
	);
	
CREATE TABLE lessons(
	pair_id integer,
	weekday_id integer,
	group_id integer,
	subject_id integer,
	class_id integer,
	teacher_id integer
	);

--Преподаватели	
INSERT INTO teachers VALUES (100, 'Juplev Anton Sergeevich');
INSERT INTO teachers VALUES (101, 'Sporyshev Maksim Sergeevich');
INSERT INTO teachers VALUES (102, 'Pak Gennadiy Konstantinovich');
INSERT INTO teachers VALUES (103, 'Klevchikhin Yuriy Aleksandrovich');
INSERT INTO teachers VALUES (104, 'Klenin Aleksandr Sergeevich');
INSERT INTO teachers VALUES (105, 'Ludov Igor Yurevich');
INSERT INTO teachers VALUES (106, 'Mashencev Vladimir Yurevich');
INSERT INTO teachers VALUES (107, 'Nikolskaya Tatiana Vladimorivna');
INSERT INTO teachers VALUES (108, 'Odin Obshchiy Fizruk');
INSERT INTO teachers VALUES (109, 'Davidiv Denis Vitalievich');
INSERT INTO teachers VALUES (110, 'Dostovalov Valeriy Nikolayevich');
INSERT INTO teachers VALUES (111, 'Shepeleva Riorita Petrovna');
INSERT INTO teachers VALUES (112, 'Romanyuk Mariya Aleksandrovna');
INSERT INTO teachers VALUES (113, 'Pak Tatiana Vladimirovna');
INSERT INTO teachers VALUES (114, 'Brizitskiy Roman Viktorovich');
INSERT INTO teachers VALUES (115, 'Pinko Irina Viktorovna');
INSERT INTO teachers VALUES (116, 'Kravcov Dmitriy Sergeevich');

--Группы
INSERT INTO groups VALUES (200, 'b8103a1');
INSERT INTO groups VALUES (201, 'b8103a2');
INSERT INTO groups VALUES (202, 'b8203a1');
INSERT INTO groups VALUES (203, 'b8203a2');

--Предметы
INSERT INTO subjects VALUES (300, 'Algebra and Geometry');
INSERT INTO subjects VALUES (301, 'Mathematical analysis');
INSERT INTO subjects VALUES (302, 'Workshop on Computer');
INSERT INTO subjects VALUES (303, 'Languages and methods of programming');
INSERT INTO subjects VALUES (304, 'PhysicalEducation');
INSERT INTO subjects VALUES (305, 'English');
INSERT INTO subjects VALUES (306, 'Discrete Mathematics');
INSERT INTO subjects VALUES (307, 'Databases');
INSERT INTO subjects VALUES (308, 'Economy');
INSERT INTO subjects VALUES (309, 'Object-oriented analysis');
INSERT INTO subjects VALUES (310, 'Physics');
INSERT INTO subjects VALUES (311, 'Differential Equations');
INSERT INTO subjects VALUES (312, 'Comprehensive analysis');
INSERT INTO subjects VALUES (313, 'Numerical Methods');
INSERT INTO subjects VALUES (314, 'Life safety');
INSERT INTO subjects VALUES (315, 'Economic theory');

--Аудитории
INSERT INTO classrooms VALUES (401, 'D401');
INSERT INTO classrooms VALUES (402, 'D402');
INSERT INTO classrooms VALUES (403, 'D403');
INSERT INTO classrooms VALUES (404, 'D404');
INSERT INTO classrooms VALUES (405, 'D405');
INSERT INTO classrooms VALUES (406, 'D406');
INSERT INTO classrooms VALUES (407, 'D407');
INSERT INTO classrooms VALUES (408, 'D408');
INSERT INTO classrooms VALUES (409, 'D409');
INSERT INTO classrooms VALUES (410, 'D410');
INSERT INTO classrooms VALUES (411, 'D411');
INSERT INTO classrooms VALUES (412, 'D412');
INSERT INTO classrooms VALUES (413, 'D413');
INSERT INTO classrooms VALUES (414, 'D414');

--Дни недели
INSERT INTO weekday VALUES (501, 'Mon');
INSERT INTO weekday VALUES (502, 'Tue');
INSERT INTO weekday VALUES (503, 'Wed');
INSERT INTO weekday VALUES (504, 'Thu');
INSERT INTO weekday VALUES (505, 'Fri');
INSERT INTO weekday VALUES (506, 'Sat');
INSERT INTO weekday VALUES (507, 'Sun');

--Пары (номера уроков на в определенный день недели)
INSERT INTO pair VALUES (1, 1);
INSERT INTO pair VALUES (2, 2);
INSERT INTO pair VALUES (3, 3);
INSERT INTO pair VALUES (4, 4);
INSERT INTO pair VALUES (5, 5);
INSERT INTO pair VALUES (6, 6);
INSERT INTO pair VALUES (7, 7);
INSERT INTO pair VALUES (8, 8);

--Отношение Группы-Предметы
INSERT INTO groups_subjects VALUES (200, 300);
INSERT INTO groups_subjects VALUES (200, 301);
INSERT INTO groups_subjects VALUES (200, 302);
INSERT INTO groups_subjects VALUES (200, 303);
INSERT INTO groups_subjects VALUES (200, 304);
INSERT INTO groups_subjects VALUES (200, 305);
INSERT INTO groups_subjects VALUES (200, 306);
INSERT INTO groups_subjects VALUES (200, 307);
INSERT INTO groups_subjects VALUES (200, 309);
INSERT INTO groups_subjects VALUES (200, 311);
INSERT INTO groups_subjects VALUES (200, 312);
INSERT INTO groups_subjects VALUES (201, 300);
INSERT INTO groups_subjects VALUES (201, 301);
INSERT INTO groups_subjects VALUES (201, 302);
INSERT INTO groups_subjects VALUES (201, 303);
INSERT INTO groups_subjects VALUES (201, 304);
INSERT INTO groups_subjects VALUES (201, 305);
INSERT INTO groups_subjects VALUES (201, 306);
INSERT INTO groups_subjects VALUES (201, 309);
INSERT INTO groups_subjects VALUES (201, 311);
INSERT INTO groups_subjects VALUES (201, 312);
INSERT INTO groups_subjects VALUES (201, 313);
INSERT INTO groups_subjects VALUES (202, 300);
INSERT INTO groups_subjects VALUES (202, 301);
INSERT INTO groups_subjects VALUES (202, 302);
INSERT INTO groups_subjects VALUES (202, 303);
INSERT INTO groups_subjects VALUES (202, 304);
INSERT INTO groups_subjects VALUES (202, 305);
INSERT INTO groups_subjects VALUES (202, 306);
INSERT INTO groups_subjects VALUES (202, 307);
INSERT INTO groups_subjects VALUES (202, 308);
INSERT INTO groups_subjects VALUES (202, 309);
INSERT INTO groups_subjects VALUES (202, 311);
INSERT INTO groups_subjects VALUES (202, 312);
INSERT INTO groups_subjects VALUES (202, 313);
INSERT INTO groups_subjects VALUES (202, 314);
INSERT INTO groups_subjects VALUES (202, 315);
INSERT INTO groups_subjects VALUES (203, 300);
INSERT INTO groups_subjects VALUES (203, 301);
INSERT INTO groups_subjects VALUES (203, 302);
INSERT INTO groups_subjects VALUES (203, 303);
INSERT INTO groups_subjects VALUES (203, 304);
INSERT INTO groups_subjects VALUES (203, 305);
INSERT INTO groups_subjects VALUES (203, 306);
INSERT INTO groups_subjects VALUES (203, 307);
INSERT INTO groups_subjects VALUES (203, 308);
INSERT INTO groups_subjects VALUES (203, 309);
INSERT INTO groups_subjects VALUES (203, 310);
INSERT INTO groups_subjects VALUES (203, 311);
INSERT INTO groups_subjects VALUES (203, 312);
INSERT INTO groups_subjects VALUES (203, 313);
INSERT INTO groups_subjects VALUES (203, 314);

--Отношение Преподаватели-Предметы
INSERT INTO teachers_subjects VALUES (100, 302);
INSERT INTO teachers_subjects VALUES (101, 303);
INSERT INTO teachers_subjects VALUES (102, 300);
INSERT INTO teachers_subjects VALUES (103, 301);
INSERT INTO teachers_subjects VALUES (104, 307);
INSERT INTO teachers_subjects VALUES (105, 312);
INSERT INTO teachers_subjects VALUES (106, 304);
INSERT INTO teachers_subjects VALUES (107, 305);
INSERT INTO teachers_subjects VALUES (108, 304);
INSERT INTO teachers_subjects VALUES (109, 308);
INSERT INTO teachers_subjects VALUES (110, 309);
INSERT INTO teachers_subjects VALUES (111, 310);
INSERT INTO teachers_subjects VALUES (112, 311);
INSERT INTO teachers_subjects VALUES (113, 313);
INSERT INTO teachers_subjects VALUES (114, 314);
INSERT INTO teachers_subjects VALUES (115, 315);
INSERT INTO teachers_subjects VALUES (116, 305);

--Первые 2 группы
--Уроки
--Понедельник
INSERT INTO lessons VALUES(1, 501, 200, 302, 401, 100);
INSERT INTO lessons VALUES(1, 501, 201, 302, 402, 106);
INSERT INTO lessons VALUES(2, 501, 200, 302, 401, 100);
INSERT INTO lessons VALUES(2, 501, 201, 302, 402, 106);
INSERT INTO lessons VALUES(3, 501, 200, 303, 403, 101);
INSERT INTO lessons VALUES(4, 501, 200, 303, 403, 101);

--Вторник
INSERT INTO lessons VALUES(1, 502, 200, 300, 401, 102);
INSERT INTO lessons VALUES(1, 502, 201, 300, 401, 102);
INSERT INTO lessons VALUES(2, 502, 200, 306, 402, 102);
INSERT INTO lessons VALUES(2, 502, 201, 306, 402, 102);
INSERT INTO lessons VALUES(3, 502, 200, 301, 403, 103);
INSERT INTO lessons VALUES(3, 502, 201, 301, 403, 103);
INSERT INTO lessons VALUES(4, 502, 200, 306, 401, 102);
INSERT INTO lessons VALUES(4, 502, 201, 306, 401, 102);

--Среда
INSERT INTO lessons VALUES(2, 503, 200, 306, 401, 102);
INSERT INTO lessons VALUES(2, 503, 201, 306, 401, 102);
INSERT INTO lessons VALUES(3, 503, 200, 300, 401, 102);
INSERT INTO lessons VALUES(3, 503, 201, 300, 401, 102);
INSERT INTO lessons VALUES(4, 503, 200, 300, 403, 102);
INSERT INTO lessons VALUES(4, 503, 201, 300, 403, 102);
INSERT INTO lessons VALUES(5, 503, 200, 304, 414, 108);
INSERT INTO lessons VALUES(5, 503, 201, 304, 414, 108);

--Четверг
INSERT INTO lessons VALUES(1, 504, 200, 307, 401, 104);
INSERT INTO lessons VALUES(1, 504, 201, 307, 401, 104);
INSERT INTO lessons VALUES(2, 504, 200, 307, 402, 104);
INSERT INTO lessons VALUES(2, 504, 201, 303, 403, 101);
INSERT INTO lessons VALUES(3, 504, 200, 303, 404, 105);
INSERT INTO lessons VALUES(3, 504, 201, 303, 404, 105);

--Пятница
INSERT INTO lessons VALUES(2, 505, 200, 301, 401, 103);
INSERT INTO lessons VALUES(2, 505, 201, 301, 401, 103);
INSERT INTO lessons VALUES(3, 505, 200, 301, 402, 103);
INSERT INTO lessons VALUES(3, 505, 201, 301, 402, 103);

--Суббота
INSERT INTO lessons VALUES(2, 506, 201, 307, 401, 104);
INSERT INTO lessons VALUES(3, 506, 201, 307, 401, 104);
INSERT INTO lessons VALUES(4, 506, 200, 305, 403, 107);
INSERT INTO lessons VALUES(4, 506, 201, 305, 403, 107);
INSERT INTO lessons VALUES(5, 506, 200, 304, 414, 108);
INSERT INTO lessons VALUES(5, 506, 201, 304, 414, 108);

--Вторые 2 группы
--Понедельник
INSERT INTO lessons VALUES(1, 501, 202, 308, 405, 109);
INSERT INTO lessons VALUES(1, 501, 203, 308, 405, 109);
INSERT INTO lessons VALUES(2, 501, 202, 308, 405, 109);
INSERT INTO lessons VALUES(2, 501, 203, 308, 405, 109);
INSERT INTO lessons VALUES(3, 501, 202, 309, 406, 100);
INSERT INTO lessons VALUES(3, 501, 203, 309, 406, 100);
INSERT INTO lessons VALUES(4, 501, 202, 302, 407, 104);
INSERT INTO lessons VALUES(4, 501, 203, 309, 408, 100);
INSERT INTO lessons VALUES(5, 501, 202, 302, 407, 104);
INSERT INTO lessons VALUES(5, 501, 203, 309, 408, 100);

--Вторник
INSERT INTO lessons VALUES(5, 502, 202, 304, 414, 108);
INSERT INTO lessons VALUES(5, 502, 203, 304, 414, 108);

--Среда
INSERT INTO lessons VALUES(1, 503, 202, 310, 406, 110);
INSERT INTO lessons VALUES(1, 503, 203, 310, 406, 110);
INSERT INTO lessons VALUES(2, 503, 202, 311, 405, 111);
INSERT INTO lessons VALUES(2, 503, 203, 311, 405, 111);
INSERT INTO lessons VALUES(3, 503, 202, 311, 405, 111);
INSERT INTO lessons VALUES(3, 503, 203, 311, 405, 111);
INSERT INTO lessons VALUES(4, 503, 202, 310, 407, 110);
INSERT INTO lessons VALUES(4, 503, 203, 310, 407, 110);
INSERT INTO lessons VALUES(5, 503, 202, 305, 406, 112);
INSERT INTO lessons VALUES(5, 503, 203, 305, 406, 112);

--Четверг
INSERT INTO lessons VALUES(1, 504, 202, 312, 405, 103);
INSERT INTO lessons VALUES(1, 504, 203, 312, 405, 103);
INSERT INTO lessons VALUES(2, 504, 202, 313, 406, 113);
INSERT INTO lessons VALUES(2, 504, 203, 313, 406, 113);
INSERT INTO lessons VALUES(3, 504, 202, 312, 407, 103);
INSERT INTO lessons VALUES(3, 504, 203, 312, 407, 103);
INSERT INTO lessons VALUES(4, 504, 202, 313, 408, 114);
INSERT INTO lessons VALUES(4, 504, 203, 313, 408, 114);
INSERT INTO lessons VALUES(5, 504, 202, 314, 409, 115);
INSERT INTO lessons VALUES(5, 504, 203, 314, 409, 115);

--Пятница
INSERT INTO lessons VALUES(1, 505, 202, 309, 405, 100);
INSERT INTO lessons VALUES(1, 505, 203, 302, 405, 104);
INSERT INTO lessons VALUES(2, 505, 202, 309, 405, 100);
INSERT INTO lessons VALUES(2, 505, 203, 302, 405, 104);
INSERT INTO lessons VALUES(3, 505, 202, 315, 406, 116);
INSERT INTO lessons VALUES(3, 505, 203, 315, 406, 116);
INSERT INTO lessons VALUES(5, 505, 202, 304, 414, 108);
INSERT INTO lessons VALUES(5, 505, 203, 304, 414, 108);

--Суббота
