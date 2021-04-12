import axios from 'axios';
import fs from 'fs';
import path from 'path';

let raw = { courses: [] };

function parseSections(data: any[]): any[] {
  const sectionsInfo = [];

  data.forEach(section => {
    const {
      section_number,
      activity: section_type,
      start: section_start,
      end: section_end,
      term: section_terms,
      days: section_days,
      instructors: section_instructors,
      totalRemaining: section_seats_remaining,
    } = section;

    const info = { 
      section_id: '',
      section_number,
      section_type,
      section_start,
      section_end,
      section_terms: section_terms.trim().split('-').map(t => parseInt(t)),
      section_days: section_days.trim().split(' '),
      section_instructors,
      section_seats_remaining: parseInt(section_seats_remaining),
    };

    sectionsInfo.push(info);
  });
  
  return sectionsInfo;
}

function parseCourses(meta: any, data: any) {
  Object.values(data).forEach((course: any) => {
    const {
      course_number,
      course_title: course_name,
      credits,
      sections,
    } = course;

    const sectionsInfo = parseSections(Object.values(sections));

    let _meta = { 
      ...meta,
      course_id: '',
      course_number,
      course_name,
      course_credits: parseInt(credits.substring(9)),
      sections: sectionsInfo,
    };

    raw.courses = [...raw.courses, _meta];
  });
}

function parseDepartment(data: any) {
  const { 
    _id: dept_id,
    code: dept_code,
    faculty: faculty_name, 
    courses,
  } = data;

  const meta = { faculty_id: '', faculty_name, dept_id, dept_code };

  parseCourses(meta, courses[0]);
}

async function main() {
  console.log('Sending request...');
  const url = 'https://ubc-courses-api.herokuapp.com/tree/2020W';
  const { data } = await axios.get(url);
  console.log('Response received');

  console.log('Parsing courses...');
  data.forEach(dept => parseDepartment(dept));
  console.log('Parsing complete');

  const filePath = path.join(__dirname, `../out/raw.json`);
  fs.writeFileSync(filePath, JSON.stringify(raw, null, 2));

  console.log('Done');
}

main();
